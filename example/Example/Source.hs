{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Example.Source where

import              Umami.Pretty

import              P

newtype Var
 = Var { getVar :: Text }
 deriving (Eq, Ord, Show)

data Exp
 = XLam Var Exp
 | XVar Var
 | XApp Exp Exp
 | XRip Exp Exp Exp
 | XLet Var Exp Exp
 | XAtom
 deriving (Eq, Ord, Show)

instance Pretty Var b where
 pretty = text . getVar

instance Pretty Exp b where
 pretty = pshaw (10 :: Int)
  where
   pshaw prec e
    = let (d,prec') = pp e
          d'  | prec < prec' && prec' >= 10
              = prefixIf line d
              | prec < prec'
              = "(" <> indent d <> ")"
              | otherwise
              = d
      in  d'

   pp = \case
    XLam x e -> ("\\" <> pretty x <> ". " <> pshaw 2 e, 2)
    XVar x   -> (pretty x, 0)
    XAtom    -> ("atom", 0)
    XApp p q -> (pshaw 1 p <+> pshaw 0 q, 1)
    XRip s p q -> ("rip" <+> pshaw 0 s <+> pshaw 0 p <+> pshaw 0 q, 2)
    XLet x p q -> ("let" <+> pretty x <+> "=" <+> indent (pshaw 2 p) <> line <> pshaw 10 q, 10)

