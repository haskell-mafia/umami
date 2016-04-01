{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Umami.Pipeline.Pipe (
    Pipe(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Section

import Umami.Monad.FreshT
import Umami.Pretty

import              P


data Pipe c u v where
 PipeNest    :: Info
             -> Pipe c u v
             -> Pipe c u v
 PipeSection :: NFData v
             => FreshFun n
             -> Section c n u v
             -> Pipe    c u v
 PipeCompose :: Pipe    c u v
             -> Pipe    c v w
             -> Pipe    c u w

instance Pretty (Pipe c u v) b where
 pretty (PipeNest i p)
  = pretty i </> indent (pretty p)
 pretty (PipeSection _ s)
  = pretty s
 pretty (PipeCompose a b)
  = pretty a </> pretty b

