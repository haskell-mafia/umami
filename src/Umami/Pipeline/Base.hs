{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Umami.Pipeline.Base (
    Id(..)
  , Info(..)
  , IdPath(..)
  , emptyPath
  , suffixPath
  ) where

import              P


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

