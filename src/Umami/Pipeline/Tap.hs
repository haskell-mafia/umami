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

data Tap m ann a
 = Tap
 { tapInfo  :: Info
 , tapOut   :: a -> m (Doc ann)
 }

newtype TapConfig
 = TapConfig
 { tapConfig :: Set.Set Id }

isTapOn :: Info -> TapConfig -> Bool
isTapOn i t
 = Set.member (infoId i) (tapConfig t)

emptyTapConfig :: TapConfig
emptyTapConfig = TapConfig Set.empty

