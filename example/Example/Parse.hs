{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Parse where

import              Example.Source

import              Umami.Pretty

import              P
import qualified    Data.Text as T


data Token
 = TVar Var
 | TParenL | TParenR
 | TLambda | TDot
 | TAtom   | TRip
 | TLet    | TEqual
 | TokError Char
 deriving (Eq, Ord, Show)

tokenise :: Text -> [Token]
tokenise = go . T.unpack
 where
  go [] = []
  go ('(':cs)
   = TParenL : go cs
  go (')':cs)
   = TParenR : go cs
  go ('\\':cs)
   = TLambda : go cs
  go ('.':cs)
   = TDot : go cs
  go ('=':cs)
   = TEqual : go cs
  go (c:cs)
   | c `elem` [' ', '\t', '\r', '\n']
   = go cs
   | c `elem` (['a'..'z'] <> ['A'..'Z'])
   = varOf (goVar cs [c])

   | otherwise
   = [TokError c]

  goVar [] ss
   = (ss,[])

  goVar (c:cs) ss
   | c `elem` (['a'..'z'] <> ['A'..'Z'])
   = goVar cs (c:ss)
   | otherwise
   = (ss, c:cs)

  varOf (ss, cs)
   = let ss' = T.pack $ reverse ss
         t'  | ss' == "atom"
             = TAtom
             | ss' == "let"
             = TLet
             | ss' == "rip"
             = TRip
             | otherwise
             = TVar $ Var ss'
      in t' : go cs


parse :: [Token] -> Either Text Exp
parse top
 = goApps top >>= checkEnd
 where
  goApps ts
   = do (t,ts') <- go ts
        goApps' t ts'
  goApps' t ts
   = case go ts of
     Left _
      -> case ts of
          (_:ts') -> return (t,ts')
          []      -> return (t,[])
     Right (t',ts') -> goApps' (XApp t t') ts'

  go [] = Left "unexpected eof"

  go (TVar v : ts)
   = return (XVar v, ts)
  go (TAtom : ts)
   = return (XAtom, ts)

  go (TRip : ts)
   = do (a,tsa) <- go ts
        (b,tsb) <- go tsa
        (c,tsc) <- go tsb
        return (XRip a b c, tsc)

  go (TLet : TVar v : TEqual : ts)
   = do (a,tsa) <- go ts
        (b,tsb) <- goApps tsa
        return (XLet v a b, tsb)

  go (TLambda : TVar v : TDot : ts)
   = do (a,tsa) <- go ts
        return (XLam v a, tsa)

  go (TParenL : ts)
   = goApps ts
  go (TParenR : _)
   = Left "unmatched close paren"

  go (er : _)
   = Left ("unexpected token: " <> T.pack (show er))

  checkEnd (r,[]) = return r
  checkEnd (_,g)  = Left ("Error: end expected at " <> T.pack (show g))


prettyToken :: Token -> Doc b
prettyToken = \case
  TVar v -> prettyVar v
  TParenL -> "("
  TParenR -> ")"
  TLambda -> "\\"
  TDot    -> "."
  TAtom   -> "atom"
  TRip    -> "rip"
  TLet    -> "let"
  TEqual  -> "="
  TokError c -> "Bad token: " <> text (T.pack $ show c)

