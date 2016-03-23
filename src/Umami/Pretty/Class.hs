{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Umami.Pretty.Class (
    Pretty(..)
  ) where

import              Umami.Pretty.Base
import              Umami.Pretty.Combinators

import              P
import qualified    Data.Text as T

class Pretty x a where
 pretty :: x -> Doc a

prettyShow :: Show a => a -> Doc b
prettyShow s = text $ T.pack $ show s

instance Pretty Int a where
 pretty = prettyShow

instance Pretty Float a where
 pretty = prettyShow

instance Pretty Double a where
 pretty = prettyShow

