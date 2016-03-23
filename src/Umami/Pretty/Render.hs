{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | "Tabular layout" version of rendering
module Umami.Pretty.Render (
    textTabsOfDoc
  , renderTabularLayout
  , renderSimpleLayout
  , renderIndentLayout
  ) where

import qualified    Umami.Pretty.Base   as B
import qualified    Umami.Pretty.Simple as S
import              Umami.Pretty.Tabular

import              P
import qualified    Data.Text as T

import              Data.List (zip, zipWith, repeat)

renderSimpleLayout :: B.RenderOptions a -> B.Doc a -> Text
renderSimpleLayout opts doc
 = go $ S.simpleDocOfDoc doc
 where
  go = \case
   S.Empty -> ""
   S.Text t r -> t    <> go r
   S.Space  r -> " "  <> go r
   S.Line   r -> "\n" <> go r
   S.Tab    r -> "\t" <> go r
   S.IndentOn   r     -> go r
   S.IndentNo   r     -> go r
   S.AnnotOn  a r -> B.roAnnotateOn opts a <> go r
   S.AnnotNo  a r -> B.roAnnotateNo opts a <> go r

renderIndentLayout :: B.RenderOptions a -> B.Doc a -> Text
renderIndentLayout opts doc
 = mconcat
 $ fmap go
 $ textTabsOfDoc opts
 $ tabDocOfSimpleDoc
 $ S.simpleDocOfDoc doc
 where
  go []     = "\n"
  go [t]    = t <> "\n"
  go (t:ts) = t <> " " <> go ts

renderTabularLayout :: B.RenderOptions a -> B.Doc a -> Text
renderTabularLayout opts doc
 = renderTabular'
 $ textTabsOfDoc opts
 $ tabDocOfSimpleDoc
 $ S.simpleDocOfDoc doc


textTabsOfDoc :: B.RenderOptions a -> TabDoc a -> [[Text]]
textTabsOfDoc opts (TabDoc lines)
 = fmap (textTabsOfLine opts) lines

textTabsOfLine :: B.RenderOptions a -> Line a -> [Text]
textTabsOfLine opts line
 = case fmap go (lineTabs line) of
    []     -> []
    (t:ts) -> indent <> t : ts
 where
  indent = T.replicate (lineIndent line) (B.roIndentText opts)

  go = fold . fmap textOfElement

  textOfElement
   = \case
      Text t    -> t
      Space     -> " "
      AnnotOn a -> B.roAnnotateOn opts a
      AnnotNo a -> B.roAnnotateNo opts a

 
renderTabular' :: [[Text]] -> Text
renderTabular' lines
 = mconcat
 $ fmap go
 $ makePadded lines
 where
  go []      = "\n"
  go [(_,t)] = t <> "\n"
  go ((w,t):ts)
   = let len    = T.length t
         padlen = w - len + 1
         pad    = T.replicate padlen " "
     in t <> pad <> go ts

makePadded :: [[Text]] -> [[(Int, Text)]]
makePadded ts
 = go [0] ts
 where
  go _ []
   = []
  go w (l:ls)
   = let len= length w

         w' | len < length l
            = -- throwlast l
              slurp len w (l:ls)
            | otherwise
            = throwlast l w
         zipped
            = (w' <> repeat 0) `zip` l

     in  zipped : go w' ls

  slurp _ w []
   = w
  slurp len w (l:ls)
   | length l <= len
   = w
   | otherwise
   = slurp len (collectWidths w l) ls

  throwlast [] _ = []
  throwlast [_] (_:_) = [0]
  throwlast (_:ts) (w:ws) = w : throwlast ts ws
  throwlast (_:ts) [] = 0 : throwlast ts []

collectWidths :: [Int] -> [Text] -> [Int]
collectWidths ws ts
 = zipWith max (ws <> repeat 0)
 $ getWidths ts


getWidths :: [Text] -> [Int]
getWidths ts
 = fmap T.length ts

{-


first line          | []        | []
foo  \t bar         | [ 3 ]     | [ 4 ]
fooo \t bazzo       | [ 4 ]     | [ 4 ]
last line           | []        | []
fop \t bobobo       | [ 3 ]     | [ 3 ]



-}
