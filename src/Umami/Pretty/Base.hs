{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Umami.Pretty.Base (
    Doc(..)
  , RenderOptions(..)
  , defaultRenderOptions
  , renderWithoutLayout
  ) where

import              P
import qualified    Data.Text as T

import              Data.String

data Doc a
 = Empty
 -- | No newlines, non-empty, non-whitespace only
 | Text !Text
 | Space
 | Line
 | Tab
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


data RenderOptions a
 = RenderOptions
 { roAnnotateOn  :: a -> Text
 , roAnnotateNo  :: a -> Text
 , roIndentText  :: Text
 }

defaultRenderOptions :: RenderOptions a
defaultRenderOptions
 = RenderOptions
 { roAnnotateOn = const ""
 , roAnnotateNo = const ""
 , roIndentText = "  " }


renderWithoutLayout :: RenderOptions a -> Doc a -> Text
renderWithoutLayout opts doc
 = fst $ go doc
 where
  go = \case
   Empty    -> ("", False)
   Text t   -> (t, True)
   Space    -> (" ", False)
   Line     -> ("\n", False)
   Tab      -> ("\t", False)
   Indent d -> go d
   CatWith l m r
    -> let (l',le) = go l
           (m', _) = go m
           (r',re) = go r
       in if   le && re
          then (l' <> m' <> r', True)
          else (l' <> r', le || re)
   Annotate a d
    -> let (d',de) = go d
       in ( roAnnotateOn opts a <> d' <> roAnnotateNo opts a
           , de)

