-- | Monad transformer for evaluating to a fixpoint
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns      #-}
module Umami.Monad.FreshT (
      FreshT   (..)
    , Fresh
    , FreshFun
    , fresh
    , evalFreshT
    , evalFresh
    ) where

import              P

import              Control.Monad.Trans.Class

import              Data.Functor.Identity


evalFreshT :: Functor m => FreshT n m a -> FreshFun n -> Int -> m a
evalFreshT m fun st
 = fst <$> runFreshT m fun st

evalFresh :: Fresh n a -> FreshFun n -> Int -> a
evalFresh m fun st
 = runIdentity $ evalFreshT m fun st


newtype FreshT n m a
 = FreshT
 { runFreshT :: FreshFun n -> Int -> m (a, Int) }

type Fresh n
 = FreshT n Identity

type FreshFun n
 = Int -> Maybe n -> n

instance Monad m => Monad (FreshT n m) where
 (>>=) p q
  = FreshT
  $ \fun st
  -> do (res,st') <- runFreshT p fun st
        runFreshT (q res) fun st'

 return a = FreshT $ \_ st -> return (a, st)

fresh :: Monad m => Maybe n -> FreshT n m n
fresh n
 = FreshT
 $ \fun st
 -> return (fun st n, st + 1)

instance Monad m => Functor (FreshT n m) where
 fmap f p
  = p >>= (return . f)

instance Monad m => Applicative (FreshT n m) where
 pure = return
 (<*>) f x
  = do !f' <- f
       !x' <- x
       return $! f' x'

instance MonadTrans (FreshT n) where
 lift m
  = FreshT
  $ \_ st
  -> do v <- m
        return (v, st)



