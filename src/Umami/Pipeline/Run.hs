{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Run (
    runPipe
  , runSection
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Pipe
import Umami.Pipeline.Section
import Umami.Pipeline.Step
import Umami.Pipeline.Tap

import Umami.Pretty
import Umami.Monad.FreshT

import              P

runPipe :: Types c => Pipe c u v -> u -> RunM c v
runPipe
 = runPipeNamed emptyQualified

runPipeNamed :: Types c => IdQualified -> Pipe c u v -> u -> RunM c v
runPipeNamed q p u
 = case p of
    PipeNest i p'
     -> runPipeNamed (suffixQualified q i) p'
    PipeCompose a b
     -> do v <- runPipeNamed q a u
           runPipeNamed q b v
    PipeSection fresh s
     -> runSection q fresh s u

runSection :: (Types c, NFData v) => IdQualified -> FreshFun n -> Section c n u v -> u -> RunM c v
runSection q fresh s u
 = do v <- runSectionPre q fresh (sectionPre s) u
      return v
      

runSectionPre :: Types c => IdQualified -> FreshFun n -> SectionPre c n u v -> u -> RunM c v
runSectionPre
 = _


 {-
 = FreshT
 $ \fresh i
 -> _
 -}
      
