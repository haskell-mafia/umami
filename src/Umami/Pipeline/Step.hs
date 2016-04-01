{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Step (
    Step(..)
  , StepRun(..)
  , StepRunOptionalConfig(..)
  , StepRunFixConfig(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Tap

import Umami.Pretty
import Umami.Monad.FixT

import              P

data Step c n a
 = Step
 { stepInfo     :: Info
 , stepRun      :: StepRun (SectionM c n) a
 , stepTaps     :: TapConfig
 }

data StepRun m a
 = StepRunMandatory (a -> m a)
 | StepRunOptional  (a -> m a) StepRunOptionalConfig 
 | StepRunFix  (a -> FixT m a) StepRunFixConfig

data StepRunOptionalConfig
 = StepRunOptionalConfig
 { stepRunOptionalEnabled :: Bool }

data StepRunFixConfig
 = StepRunFixConfig
 { stepRunFixEnabled            :: Bool
 -- I think this would be useful later
 -- , stepRunFixShowEveryIteration :: Bool
 }


instance Pretty (Step c n a) b where
 pretty s
  =   pretty (stepInfo s) </> pretty (stepRun s) </> taps
  where
   taps
    = prefixIf (" taps: " <> tab)
               (pretty $ stepTaps s)

instance Pretty (StepRun m a) b where
  pretty (StepRunMandatory _)
   = emptyDoc

  pretty (StepRunOptional _ c)
   | stepRunOptionalEnabled c
   = "(optional)"
   | otherwise
   = "(disabled)"

  pretty (StepRunFix _ c)
   = "(fixpoint)"
   <> ift (not $ stepRunFixEnabled c) "(disabled)"
   where
    ift b d
     = if   b
       then d
       else emptyDoc

