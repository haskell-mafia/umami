-- | Just some junk for generating arbitrary instances
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Umami.Pretty.Arbitrary where

import           Disorder.Corpus
import           Test.QuickCheck

import           P


import Umami.Pretty.Base

gen_doc :: Gen a -> Gen (Doc a)
gen_doc ann
 = oneof_sized
    [ return Empty
    , Text <$> elements waters
    , return Space
    , return Line ]
    [ Indent <$> go
    -- Assume the joiner is white-space only
    , CatWith <$> go <*> return Space <*> go
    , Annotate <$> ann <*> go ]
 where
  go = gen_doc ann


-- I really must add these to Disorder

-- | Make a smaller generator
smaller :: Gen a -> Gen a
smaller g
 = sized
 $ \s -> resize (s `div` 2) g

-- | Take list of small generators and list of large generators.
-- Look at the size of thing we want to create, and use either small or both
oneof_sized :: [Gen a] -> [Gen a] -> Gen a
oneof_sized smalls bigs
 = sized
 $ \s -> if   s <= 1
         then oneof  smalls
         else oneof (smalls <> bigs')
 where
  bigs'   = fmap smaller bigs

