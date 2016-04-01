{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Tap (
    Tap(..)
  , TapConfig(..)
  ) where

import Umami.Pipeline.Base
import Umami.Pretty

import              P

data Tap c n a
 = Tap
 { tapInfo  :: Info
 , tapOut   :: a -> SectionM c n (Doc (TypesAnnotation c))
 }

newtype TapConfig
 = TapConfig
 { tapConfig :: [Id] }

instance Pretty (Tap c n a) b where
 pretty = pretty . tapInfo

instance Pretty TapConfig b where
 pretty = punctuate ", " . fmap pretty . tapConfig


