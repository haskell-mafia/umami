{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Umami.Pipeline.Pretty (
    prettyId
  , prettyInfo
  , prettyIdPath
  , prettySection
  , prettyPipe
  , prettyStep
  , prettyStepRun
  , prettyTap
  , prettyTapConfig
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Pipe
import Umami.Pipeline.Section
import Umami.Pipeline.Step
import Umami.Pipeline.Tap

import Umami.Pretty

import qualified Data.Set as Set
import P

prettyId :: Id -> Doc b
prettyId = text . idName

prettyInfo :: Info -> Doc b
prettyInfo t = prettyId (infoId t) <> ":" <#> text (infoDesc t)

prettyIdPath :: IdPath -> Doc b
prettyIdPath (IdPath is)
  = punctuate "."
  $ fmap prettyId
  $ reverse is

prettySection :: Section m b n u v -> Doc b
prettySection s
  =
  vcat 
  [ prettyInfo (sectionInfo s)
  , prefix " initial taps:"   (prettyTapConfig $ sectionPreTaps $ sectionPre s)
  , prefix " further steps:"  (vcat $ fmap prettyStep $ sectionSteps s)
  , prefix " available taps:" (vcat $ fmap prettyTap $ sectionTaps s)
  ]
  where
   prefix l r
    = prefixIf (l <> line) (indent r)


prettyPipe :: Pipe m b u v -> Doc b
prettyPipe (PipeNest i p)
  = prettyInfo i </> indent (prettyPipe p)
prettyPipe (PipeSection _ s)
  = prettySection s
prettyPipe (PipeCompose a b)
  = prettyPipe a </> prettyPipe b


prettyStep :: Step m n a -> Doc b
prettyStep s
  = prettyInfo (stepInfo s) </> prettyStepRun (stepRun s) </> taps
  where
   taps
    = prefixIf (" taps: " <> tab)
               (prettyTapConfig $ stepTaps s)

prettyStepRun :: StepRun m a -> Doc b
prettyStepRun (StepRunMandatory _)
   = emptyDoc

prettyStepRun (StepRunOptional _ c)
   | stepRunOptionalEnabled c
   = "(optional)"
   | otherwise
   = "(disabled)"

prettyStepRun (StepRunFix _ c)
   = "(fixpoint)"
   <> ift (not $ stepRunFixEnabled c) "(disabled)"
   where
    ift b d
     = if   b
       then d
       else emptyDoc

prettyTap :: Tap m b a -> Doc b
prettyTap = prettyInfo . tapInfo

prettyTapConfig :: TapConfig -> Doc b
prettyTapConfig = punctuate ", " . fmap prettyId . Set.toList . tapConfig


