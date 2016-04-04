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

data RunConfig c m
 = RunConfig
 { runConfigLift   :: forall a. IdPath -> PipeM c a -> m a
 , runConfigPutDoc :: IdPath -> Doc (TypesAnnotation c) -> m ()
 }

liftFresh :: Types c => RunConfig c m -> IdPath -> SectionM c n a -> FreshT n m a
liftFresh run path m
 = FreshT
 $ \freshfun i
 -> runConfigLift run path $ runFreshT m freshfun i




runPipe :: (Types c, Monad m) => RunConfig c m -> Pipe c u v -> u -> m v
runPipe 
 = flip runPipeNamed emptyPath

runPipeNamed :: (Types c, Monad m) => RunConfig c m -> IdPath -> Pipe c u v -> u -> m v
runPipeNamed run q p u
 = case p of
    PipeNest i p'
     -> runPipeNamed run (suffixPath q i) p' u
    PipeCompose a b
     -> do v <- runPipeNamed run q a u
           runPipeNamed run q b v
    PipeSection freshfun s
     -> runSection run q freshfun s u

runSection :: (Types c, Monad m) => RunConfig c m -> IdPath -> FreshFun n -> Section c n u v -> u -> m v
runSection run q freshfun s u
 = evalFreshT go freshfun 0
 where
  q' = suffixPath q (sectionInfo s)
  go = do v <- runSectionPre run q' (sectionPre s) u
          runTaps run q' (sectionTaps s) (sectionPreTaps $ sectionPre s) v
          foldM goStep v (sectionSteps s)

  goStep u' step
   = runStep run q' (sectionTaps s) step u'
      

runSectionPre :: (Monad m, Types c) => RunConfig c m -> IdPath -> SectionPre c n u v -> u -> FreshT n m v
runSectionPre run q s u 
 = liftFresh run q
 $ sectionPreRun s u

runStep :: (Monad m, Types c) => RunConfig c m -> IdPath -> [Tap c n u] -> Step c n u -> u -> FreshT n m u
runStep run q taps s u 
 = do u' <- liftFresh run q' (go $ stepRun s)
      runTaps run q' taps (stepTaps s) u'
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


runTaps :: (Monad m, Types c) => RunConfig c m -> IdPath -> [Tap c n u] -> TapConfig -> u -> FreshT n m ()
runTaps run q taps conf u
 = mapM_ go taps
 where
  go t
   | i  <- tapInfo t
   , q' <- suffixPath q i
   , isTapOn i conf
   = do out <- liftFresh run q' $ tapOut t u
        lift $ runConfigPutDoc run q' out

   | otherwise
   = return ()
