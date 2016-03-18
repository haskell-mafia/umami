{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Faster version of rendering
module Umami.Pretty.Render (
    renderM
  , renderText
  , RenderOptions(..)
  , defaultRenderOptions
  ) where

import              Umami.Pretty.Simple

import              P
import qualified    Data.Text as T
import              Control.Monad.Trans.State

data RenderOptions a
 = RenderOptions
 { roAnnotateOn  :: a -> T.Text
 , roAnnotateNo  :: a -> T.Text
 , roIndentText  :: T.Text
 }

defaultRenderOptions :: RenderOptions a
defaultRenderOptions
 = RenderOptions
 { roAnnotateOn = const ""
 , roAnnotateNo = const ""
 , roIndentText = "  " }


data RenderState
 = RenderState
 { rsStartOfLine :: !Bool
 , rsIndentLevel :: !Int
 , rsIndents     :: [Bool]
 , rsIndentedYet :: !Bool
 }


renderText :: RenderOptions a -> SimpleDoc a -> T.Text
renderText opts d
 = execState (renderM opts app d) ""
 where
  app t = modify (\s -> s <> t)


renderM :: Monad m => RenderOptions a -> (T.Text -> m b) -> SimpleDoc a -> m b
renderM opts f
 = go (RenderState True 0 [] False)
 where

  fText s t r
   | rsStartOfLine s
   = f (T.replicate (rsIndentLevel s) (roIndentText opts) <> t)
   >> go s { rsStartOfLine = False, rsIndentedYet = False } r
   | otherwise
   = f t
   >> go s r

  -- Keep track of all document before and after
  -- inside current indent
  go s
   = \case
      Empty
       -> f ""
      Text t r
       -> fText s t r
      Space  r
       | rsStartOfLine s
       -> go s r
       | otherwise
       -> f " " >> go s r
      Line   r
       ->  f "\n"
       >> go (s { rsStartOfLine = True }) r

      IndentOn r
       -> let shift = not (rsIndentedYet s)
              level'| shift
                    = rsIndentLevel s + 1
                    | otherwise
                    = rsIndentLevel s
              indents' = shift : rsIndents s
              s'    = s { rsIndentedYet = True, rsIndentLevel = level', rsIndents = indents' }
          in go s' r

      IndentNo r
       -> let (sh,is') = case rsIndents s of
                          [] -> (False, [])
                          (i:is) -> (i,is)
              level'   | sh
                       = rsIndentLevel s - 1
                       | otherwise
                       = rsIndentLevel s

              s' = s { rsIndentedYet = not sh, rsIndentLevel = level', rsIndents = is' }
          in go s' r

      AnnotOn a r
       -> fText s (roAnnotateOn opts a) r

      AnnotNo a r
       -> fText s (roAnnotateNo opts a) r

