{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module Example.Pipeline where

import Example.Parse
import Example.Source

import              Umami.Pretty
import qualified    Umami.Pretty.Render  as PR

import              Umami.Pipeline

import              P
import qualified    Data.Text as T

import              Data.Functor.Identity
import              Control.Monad.Trans.Class
import              Control.Monad.Trans.Either

import qualified    Data.Text.IO  as TIO
import              System.IO     (IO)
import qualified    System.IO     as IO

import qualified    Data.Set as Set


data ExamplePipe

instance Types ExamplePipe where
 type TypesMonad      ExamplePipe = Identity
 type TypesError      ExamplePipe = Text
 type TypesAnnotation ExamplePipe = ()

pipe :: Pipe ExamplePipe Text Exp
pipe
 = PipeSection dummyFresh sectionToken
 `PipeCompose`
   PipeSection dummyFresh sectionParse

 where
  dummyFresh _ _ = Var ""

sectionToken :: Section ExamplePipe Var Text [Token]
sectionToken
 = Section
 { sectionInfo = info "token" "Tokenise from Text"
 , sectionPre  = sectionDo (return . tokenise)
 , sectionSteps= []
 , sectionTaps = [tapShow] }


sectionParse :: Section ExamplePipe Var [Token] Exp
sectionParse
 = Section
 { sectionInfo = info "parse" "Parse from tokens"
 , sectionPre  = sectionDo parse'
 , sectionSteps= []
 , sectionTaps = [tapShow, tapPretty] }
 where
  parse' :: [Token] -> SectionM ExamplePipe Var Exp
  parse' t
   = lift (hoistEither $ parse t)


runConfig :: RunConfig ExamplePipe (EitherT (IdPath,Text) IO)
runConfig
 = RunConfig
 { runConfigLift   = runlift
 , runConfigPutDoc = runput }
 where
  runlift ip m
   = case runIdentity $ runEitherT m of
     Left e -> left (ip,e)
     Right r -> return r

  runput ip d
   = lift
   $ TIO.putStr
   $ PR.renderTabularLayout defaultRenderOptions
   ( "At " <> pretty ip <> ":" <> line <> indent d <> line)

run :: (Show a, Show b) => Pipe ExamplePipe a b -> a -> IO ()
run p a
 = do r <- runEitherT $ runPipe runConfig p a
      IO.putStrLn $ either show show r


tapShow :: Show a => Tap ExamplePipe Var a
tapShow
 = Tap
 { tapInfo = info "show" "Use builtin Show instance"
 , tapOut  = return . text . T.pack . show
 }

tapPretty :: Pretty a () => Tap ExamplePipe Var a
tapPretty
 = Tap
 { tapInfo = info "pretty" "Use Pretty instance"
 , tapOut  = return . pretty
 }



sectionDo :: (u -> SectionM c n v) -> SectionPre c n u v
sectionDo f
 = SectionPre f (TapConfig $ Set.fromList [Id "show", Id "pretty"])

info :: Text -> Text -> Info
info id' desc = Info (Id id') desc

