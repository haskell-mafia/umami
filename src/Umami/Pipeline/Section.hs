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


-- | A straight section of pipe, that works over the same type.
-- A section can define multiple steps to perform, but they must all be of (a -> a).
-- Then, all steps can use the same traces/taps and so on.
data Section c n u v
 = Section
 { sectionInfo  :: Info
 , sectionPre   :: SectionPre c n u v
 , sectionSteps :: [Step c n v]
 , sectionTaps  :: [Tap  c n v] }

data SectionPre c n u v
 = SectionPre
 { sectionPreRun    :: u -> SectionM c n v
 , sectionPreTaps   :: TapConfig }
