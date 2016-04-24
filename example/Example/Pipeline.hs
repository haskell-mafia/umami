{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module Example.Pipeline where

import              Example.Parse
import              Example.Source

import              Umami.Monad.FreshT
import              Umami.Pretty
import qualified    Umami.Pretty.Render  as PR

import              Umami.Pipeline

import              P
import qualified    Data.Text as T

import              Control.Monad.Trans.Class
import              Control.Monad.Trans.Either

import qualified    Data.Text.IO  as TIO
import              System.IO     (IO)
import qualified    System.IO     as IO

import qualified    Data.Set as Set


type ExampleM = Either Text

pipe :: Pipe ExampleM () Text Exp
pipe
 = PipeSection dummyFresh sectionToken
 `PipeCompose`
   PipeSection dummyFresh sectionParse

 where
  dummyFresh _ _ = Var ""

sectionToken :: Section ExampleM () Var Text [Token]
sectionToken
 = Section
 { sectionInfo = info "token" "Tokenise from Text"
 , sectionPre  = sectionDo (return . tokenise)
 , sectionSteps= []
 , sectionTaps = [tapShow] }


sectionParse :: Section ExampleM () Var [Token] Exp
sectionParse
 = Section
 { sectionInfo = info "parse" "Parse from tokens"
 , sectionPre  = sectionDo parse'
 , sectionSteps= []
 , sectionTaps = [tapShow, tapPretty prettyExp] }
 where
  parse' :: [Token] -> FreshT Var ExampleM Exp
  parse' t
   = lift $ parse t


runConfig :: RunConfig ExampleM (EitherT (IdPath,Text) IO) ()
runConfig
 = RunConfig
 { runConfigLift   = runlift
 , runConfigPutDoc = runput }
 where
  runlift ip m
   = case m of
     Left e -> left (ip,e)
     Right r -> return r

  runput ip d
   = lift
   $ TIO.putStr
   $ PR.renderTabularLayout defaultRenderOptions
   ( "At " <> prettyIdPath ip <> ":" <> line <> indent d <> line)

run :: (Show a, Show b) => Pipe ExampleM () a b -> a -> IO ()
run p a
 = do r <- runEitherT $ runPipe runConfig p a
      IO.putStrLn $ either show show r


tapShow :: Show a => Tap ExampleM () a
tapShow
 = Tap
 { tapInfo = info "show" "Use builtin Show instance"
 , tapOut  = return . text . T.pack . show
 }

tapPretty :: (a -> Doc ()) -> Tap ExampleM () a
tapPretty f
 = Tap
 { tapInfo = info "pretty" "Use Pretty instance"
 , tapOut  = return . f
 }



sectionDo :: (u -> FreshT n m v) -> SectionPre m n u v
sectionDo f
 = SectionPre f (TapConfig $ Set.fromList [Id "show", Id "pretty"])

info :: Text -> Text -> Info
info id' desc = Info (Id id') desc

