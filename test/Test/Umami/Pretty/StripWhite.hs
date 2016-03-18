-- | Just some junk for generating arbitrary instances
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Umami.Pretty.StripWhite where

import Test.Umami.Pretty.Arbitrary

import           Umami.Pretty.Base   as PB
import qualified Umami.Pretty.Render as PR
import qualified Umami.Pretty.Simple as PS

import           Test.QuickCheck

import           P

import System.IO

import qualified Data.Text as T

renderText :: PB.Doc a -> Text
renderText d
 = let d' = PS.simpleDocOfDoc d
   in  PR.renderText PR.defaultRenderOptions d'

renderDumb :: PB.Doc a -> Text
renderDumb d
 = go d
 where
  go Empty = ""
  go (Text t) = t
  go Space = " "
  go Line = "\n"
  go (Indent i) = go i
  go (CatWith a b c)
   = let a' = go a
         b' = go b
         c' = go c
     in  a' <> b' <> c'
  go (Annotate _ i) = go i

prop_render_strip
 = forAll (gen_doc (return ()))
 $ \d
 -> let rend = renderText d
        dumb = renderDumb d
    in T.words rend === T.words dumb


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 100, maxDiscardRatio = 100})

