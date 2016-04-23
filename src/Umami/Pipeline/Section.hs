{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Section (
    Section(..)
  , SectionPre(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Step
import Umami.Pipeline.Tap

import Umami.Monad.FreshT


-- | A straight section of pipe, that works over the same type.
-- A section can define multiple steps to perform, but they must all be of (a -> a).
-- Then, all steps can use the same traces/taps and so on.
data Section m ann n u v
 = Section
 { sectionInfo  :: Info
 , sectionPre   :: SectionPre m n u v
 , sectionSteps :: [Step m n v]
 , sectionTaps  :: [Tap  m ann v] }

data SectionPre m n u v
 = SectionPre
 { sectionPreRun    :: u -> FreshT n m v
 , sectionPreTaps   :: TapConfig }
