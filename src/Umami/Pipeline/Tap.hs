{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
module Umami.Pipeline.Tap (
    Tap(..)
  , TapConfig(..)
  , isTapOn
  , emptyTapConfig
  ) where

import Umami.Pipeline.Base

import Umami.Pretty

import              P

import qualified    Data.Set as Set

data Tap c n a
 = Tap
 { tapInfo  :: Info
 , tapOut   :: a -> SectionM c n (Doc (TypesAnnotation c))
 }

newtype TapConfig
 = TapConfig
 { tapConfig :: Set.Set Id }

isTapOn :: Info -> TapConfig -> Bool
isTapOn i t
 = Set.member (infoId i) (tapConfig t)

emptyTapConfig :: TapConfig
emptyTapConfig = TapConfig Set.empty

