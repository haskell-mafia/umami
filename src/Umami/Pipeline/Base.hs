{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Umami.Pipeline.Base (
    Types(..)
  , PipeM
  , SectionM
  , Id(..)
  , Info(..)
  , IdPath(..)
  , emptyPath
  , suffixPath
  ) where

import Umami.Monad.FreshT

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

data IdPath
 = IdPath [Id]
 deriving (Eq, Show)

emptyPath :: IdPath
emptyPath = IdPath []

suffixPath :: IdPath -> Info -> IdPath
suffixPath (IdPath is) i = IdPath (infoId i : is)

