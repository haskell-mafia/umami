{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Umami.Pipeline.Pipe (
    Pipe(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pipeline.Section

import Umami.Monad.FreshT


data Pipe m ann u v where
 PipeNest    :: Info
             -> Pipe    m ann u v
             -> Pipe    m ann u v
 PipeSection :: FreshFun n
             -> Section m ann n u v
             -> Pipe    m ann u v
 PipeCompose :: Pipe    m ann u v
             -> Pipe    m ann v w
             -> Pipe    m ann u w

