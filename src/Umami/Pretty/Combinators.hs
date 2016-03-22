{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Umami.Pretty.Combinators (
    empty, text, line, space, indent
  , (</>), (<+>)
  , hsep, vcat, hcat
  , annotate

  , encloseSep, punctuate, fold, joined
  ) where

import              Umami.Pretty.Base

import              P hiding (empty, fold)


{-
list :: [Doc a] -> Doc a
list            = encloseSep lbracket rbracket comma

tupled :: [Doc a] -> Doc a
tupled          = encloseSep lparen   rparen  comma

semiBraces :: [Doc a] -> Doc a
semiBraces      = encloseSep lbrace   rbrace  semi

-}

hsep :: [Doc a] -> Doc a
hsep = fold (<+>)

vcat :: [Doc a] -> Doc a
vcat = fold (</>)

hcat :: [Doc a] -> Doc a
hcat = fold (<>)


empty :: Doc a
empty = Empty

text :: Text -> Doc a
text = Text

line :: Doc a
line = Line

space :: Doc a
space = Space

indent :: Doc a -> Doc a
indent = Indent


infixr 5 </>
infixr 6 <+>

(</>) :: Doc a -> Doc a -> Doc a
(</>) = joined line

(<+>) :: Doc a -> Doc a -> Doc a
(<+>) = joined space

joined :: Doc a -> Doc a -> Doc a -> Doc a
joined m l r = CatWith l m r

annotate :: a -> Doc a -> Doc a
annotate a d = Annotate a d


fold :: (Doc a -> Doc a -> Doc a) -> [Doc a] -> Doc a
fold _ []  = empty
fold _ [d] = d
fold f (d:ds)
 = f d (fold f ds)

punctuate :: Doc a -> [Doc a] -> Doc a
punctuate _ []      = empty
punctuate _ [d]     = d
punctuate p (d:ds)  = CatWith d p (punctuate d ds)


encloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
encloseSep left right sep ds
 = left <> punctuate sep ds <> right

