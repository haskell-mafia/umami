{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Umami.Pipeline.Base (
    Types(..)
  , PipeM
  , SectionM
  , Id(..)
  , Info(..)
  , IdQualified(..)
  , emptyQualified
  , suffixQualified
  ) where

import Umami.Monad.FreshT
import Umami.Pretty

import              P
import Control.Monad.Trans.Either

-- | This is not strictly necessary, nor is it what you would call a "real typeclass".
-- It is simply for convenience.
class Monad (TypesMonad c) => Types c where
 type TypesMonad      c   :: * -> *
 type TypesError      c   :: *
 type TypesAnnotation c   :: *

type PipeM c
 = EitherT (TypesError c) (TypesMonad c)

type SectionM c n
 = FreshT n (PipeM c)


newtype Id
 = Id
 { idName :: Text }
 deriving (Eq, Ord, Show)

data Info
 = Info
 { infoId   :: Id
 , infoDesc :: Text }
 deriving Show

data IdQualified
 = IdQualified [Id]
 deriving (Eq, Show)

emptyQualified :: IdQualified
emptyQualified = IdQualified []

suffixQualified :: IdQualified -> Id -> IdQualified
suffixQualified (IdQualified is) i = IdQualified (i : is)


instance Pretty Id b where
 pretty = text . idName

instance Pretty Info b where
 pretty t = pretty (infoId t) <> ":" <#> text (infoDesc t)

instance Pretty IdQualified b where
 pretty (IdQualified is)
  = punctuate "."
  $ fmap pretty is

