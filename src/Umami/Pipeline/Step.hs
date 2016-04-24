{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Step (
    Step(..)
  , StepRun(..)
  , StepRunOptionalConfig(..)
  , StepRunFixConfig(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Tap

import Umami.Monad.FixT
import Umami.Monad.FreshT

import P

data Step m n a
 = Step
 { stepInfo     :: Info
 , stepRun      :: StepRun (FreshT n m) a
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

