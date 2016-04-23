{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
module Umami.Pipeline.Run (
    RunConfig(..)
  , runPipe
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Pipe
import Umami.Pipeline.Section
import Umami.Pipeline.Step
import Umami.Pipeline.Tap

import Umami.Pretty
import Umami.Monad.FreshT
import Umami.Monad.FixT

import              P
import              Control.Monad.Trans.Class


data RunConfig m m' ann
 = RunConfig
 { runConfigLift   :: forall a. IdPath -> m a -> m' a
 , runConfigPutDoc :: IdPath -> Doc ann -> m' ()
 }

runConfigLiftF :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> FreshT n m a -> FreshT n m' a
runConfigLiftF run q m
 = FreshT
 $ \f i
 -> runConfigLift run q
  $ runFreshT m f i

runPipe :: (Monad m, Monad m') => RunConfig m m' ann -> Pipe m ann u v -> u -> m' v
runPipe 
 = flip runPipeNamed emptyPath

runPipeNamed :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> Pipe m ann u v -> u -> m' v
runPipeNamed run q p u
 = case p of
    PipeNest i p'
     -> runPipeNamed run (suffixPath q i) p' u
    PipeCompose a b
     -> do v <- runPipeNamed run q a u
           runPipeNamed run q b v
    PipeSection freshfun s
     -> runSection run q freshfun s u

runSection :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> FreshFun n -> Section m ann n u v -> u -> m' v
runSection run q freshfun s u
 = evalFreshT go freshfun 0
 where
  q' = suffixPath q (sectionInfo s)
  go = do v <- runSectionPre run q' (sectionPre s) u
          lift $ runTaps run q' (sectionTaps s) (sectionPreTaps $ sectionPre s) v
          foldM goStep v (sectionSteps s)

  goStep u' step
   = runStep run q' (sectionTaps s) step u'
      

runSectionPre :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> SectionPre m n u v -> u -> FreshT n m' v
runSectionPre run q s u 
 = runConfigLiftF run q
 $ sectionPreRun s u

runStep :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> [Tap m ann u] -> Step m n u -> u -> FreshT n m' u
runStep run q taps s u 
 = do u' <- runConfigLiftF run q $ go $ stepRun s
      lift $ runTaps run q' taps (stepTaps s) u'
      return u'
 where
  q' = suffixPath q $ stepInfo s
  
  go (StepRunMandatory m)
   = m u

  go (StepRunOptional m opt)
   | stepRunOptionalEnabled opt
   = m u
   | otherwise
   = return u

  go (StepRunFix m opt)
   | stepRunFixEnabled opt
   = fixpoint m u
   | otherwise
   = return u


runTaps :: (Monad m, Monad m') => RunConfig m m' ann -> IdPath -> [Tap m ann u] -> TapConfig -> u -> m' ()
runTaps run q taps conf u
 = mapM_ go taps
 where
  go t
   | i  <- tapInfo t
   , q' <- suffixPath q i
   , isTapOn i conf
   = do out <- runConfigLift run q' $ tapOut t u
        runConfigPutDoc run q' out

   | otherwise
   = return ()
