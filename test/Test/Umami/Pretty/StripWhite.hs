-- | Just some junk for generating arbitrary instances
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Umami.Pretty.StripWhite where

import Test.Umami.Pretty.Arbitrary

import           Umami.Pretty.Base   as PB
import qualified Umami.Pretty.Render as PR

import           Test.QuickCheck

import           P

import System.IO

import qualified Data.Text as T

prop_render_simple_layout
 = forAll (gen_doc (return ()))
 $ check_layout PR.renderSimpleLayout

prop_render_indent_layout
 = forAll (gen_doc (return ()))
 $ check_layout PR.renderIndentLayout

prop_render_tab_layout
 = forAll (gen_doc (return ()))
 $ check_layout PR.renderTabularLayout

check_layout lay d
 =  let rend = lay                    PB.defaultRenderOptions d
        dumb = PB.renderWithoutLayout PB.defaultRenderOptions d
    in counterexample (show rend)
     $ counterexample (show dumb)
     $ T.words rend === T.words dumb

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10000, maxSize = 100, maxDiscardRatio = 100})

