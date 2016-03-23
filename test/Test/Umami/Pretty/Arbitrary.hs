-- | Just some junk for generating arbitrary instances
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Umami.Pretty.Arbitrary where

import           Disorder.Core
import           Disorder.Corpus
import           Test.QuickCheck

import           P
import qualified Data.Text as T


import Umami.Pretty.Base

gen_doc :: Gen a -> Gen (Doc a)
gen_doc ann
 = oneofSized
    [ return Empty
    -- Try with some 'normal' textual inputs 
    , Text <$> elements waters
    -- But add some totally crazy stuff in there too
    , Text . T.pack <$> arbitrary
    , return Space
    , return Tab
    , return Line ]
    [ Indent    <$> go
    , CatWith   <$> go  <*> go <*> go
    , Annotate  <$> ann <*> go ]
 where
  go = gen_doc ann


