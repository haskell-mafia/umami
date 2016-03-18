{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Umami.Pretty.Base (
    Doc(..)
  ) where

import              P
import qualified    Data.Text as T

import              Data.String

data Doc a
 = Empty
 -- | No newlines, non-empty, non-whitespace only
 | Text !T.Text
 | Space
 | Line
 | Indent (Doc a)
 -- | CatWith l m r joins l and r, separated by m
 -- if l and r are both non-empty non-white
 | CatWith (Doc a) (Doc a) (Doc a)
 | Annotate !a (Doc a)
 deriving (Eq, Ord, Show, Functor)

instance Monoid (Doc a) where
 mempty      = Empty
 mappend l r = CatWith l Empty r

instance IsString (Doc a) where
    fromString = Text . T.pack

