{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Section (
    Section(..)
  , SectionPre(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Step
import Umami.Pipeline.Tap

import Umami.Pretty

import              P

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

instance Pretty (Section c n u v) b where
 pretty s
  =
  vcat 
  [ pretty (sectionInfo s)
  , prefix " initial taps:"   (pretty $ sectionPreTaps $ sectionPre s)
  , prefix " further steps:"  (vcat $ fmap pretty $ sectionSteps s)
  , prefix " available taps:" (vcat $ fmap pretty $ sectionTaps s)
  ]
  where
   prefix l r
    = prefixIf (l <> line) (indent r)

