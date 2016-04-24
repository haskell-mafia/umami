{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Example.Pipeline

import              Umami.Pipeline
import              Umami.Pretty
import qualified    Umami.Pretty.Render  as PR
import qualified    Umami.Pretty.Simple  as PS
import qualified    Umami.Pretty.Tabular as PT

import              P

import              System.IO           (IO)
import              System.Environment  (getArgs)

import qualified    Data.Text as T
import qualified    Data.Text.IO  as TIO


main :: IO ()
main
 = do   args <- getArgs
        render (prettyPipe pipe)
        let args' = T.unwords $ fmap T.pack args
        run pipe args'

render :: Doc a -> IO ()
render d
 = TIO.putStrLn
 $ PR.renderTabularLayout defaultRenderOptions d

