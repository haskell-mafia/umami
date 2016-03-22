{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Umami.Pretty.Tabular (
    TabDoc(..), Line(..), Element(..)
  , tabDocOfSimpleDoc
  ) where

import qualified    Umami.Pretty.Simple as S

import              P

data TabDoc a
 = TabDoc [Line a]
 deriving (Eq, Ord, Show)

data Line a
 = Line
 { lineIndent :: !Int
 , lineTabs   :: [[Element a]]
 }
 deriving (Eq, Ord, Show)

data Element a
 = Text !Text
 | Space
 | AnnotOn  !a
 | AnnotNo  !a
 deriving (Eq, Ord, Show)

tabDocOfSimpleDoc :: S.SimpleDoc a -> TabDoc a
tabDocOfSimpleDoc
 = TabDoc . convert initialState

convert :: State a -> S.SimpleDoc a -> [Line a]
convert s
 = \case
    S.Empty       -> [emit]
    S.Text    t r -> convert (insert $ Text t)    r
    S.Space     r -> convert (insert   Space)     r
    S.AnnotOn a r -> convert (insert $ AnnotOn a) r
    S.AnnotNo a r -> convert (insert $ AnnotNo a) r

    S.Line      r -> emit : convert flushL        r
    S.Tab       r ->        convert (flushTab s)  r

    S.IndentOn  r ->        convert (indentOn s)  r
    S.IndentNo  r ->        convert (indentNo s)  r

 where
  emit = fst $ emitLine s
  insert = insertElement s
  flushL = snd $ emitLine s


data State a
 = State
 { stateTab     :: [Element a]
 , stateLine    :: [[Element a]]
 , stateIndentThis  :: !Int
 , stateIndentNext  :: !Int
 , stateIndentStack :: [Bool]
 }
 deriving (Eq, Ord, Show)


initialState :: State a
initialState = State [] [] 0 0 []

emitLine :: State a -> (Line a, State a)
emitLine s
 = ( Line (stateIndentThis s) (reverse $ stateLine $ flushTab s)
   , s { stateLine       = []
       , stateTab        = []
       , stateIndentThis = stateIndentNext s } )

flushTab :: State a -> State a
flushTab s
 = s
 { stateLine = reverse (stateTab s) : stateLine s
 , stateTab  = [] }

insertElement :: State a -> Element a -> State a
insertElement s e
 = s
 { stateTab  = insert' e (stateTab s) }
 where
  insert' Space []           = []
  insert' Space (Space : ts) = (Space : ts)
  insert' _     ts           = (e     : ts)


indentOn :: State a -> State a
indentOn s
 = let eff  = stateIndentNext s <= stateIndentThis s
       eff' = if eff then 1 else 0
   in s
      { stateIndentNext  = stateIndentNext s + eff'
      , stateIndentStack = eff : stateIndentStack s }

indentNo :: State a -> State a
indentNo s
 = case stateIndentStack s of
    True : st
     -> s { stateIndentStack = st, stateIndentNext = stateIndentNext s - 1 }
    False : st
     -> s { stateIndentStack = st }
    []
     -> s

