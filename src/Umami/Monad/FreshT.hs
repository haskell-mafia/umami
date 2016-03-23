-- | Fresh variable plumbing:
-- Monad transformer for supplying new variable names.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns      #-}
module Umami.Monad.FreshT (
      evalFreshT
    , evalFresh
    , FreshT   (..)
    , Fresh
    , FreshFun
    , fresh
    ) where

import              P

import              Control.Monad.Trans.Class

import              Data.Functor.Identity


-- | Evaluate fresh transformer
-- Takes the action to perform, as well as a generator function and a starting number (probably zero)
evalFreshT :: Functor m => FreshT n m a -> FreshFun n -> Int -> m a
evalFreshT m fun st
 = fst <$> runFreshT m fun st

-- | Evaluate fresh monad
evalFresh :: Fresh n a -> FreshFun n -> Int -> a
evalFresh m fun st
 = runIdentity $ evalFreshT m fun st


-- | Fresh variable plumbing transformer
-- We may like to change this to a StateT or an IORef at some stage.
newtype FreshT n m a
 = FreshT
 { runFreshT :: FreshFun n -> Int -> m (a, Int) }

-- | Fresh variable monad
type Fresh n
 = FreshT n Identity

-- | Generator function for fresh names.
-- Takes an Int, which is the "variable number" for this step,
-- and an optional name to base it on.
-- The idea is that if a name is given, the new name can use this as a prefix.
type FreshFun n
 = Int -> Maybe n -> n

instance Monad m => Monad (FreshT n m) where
 (>>=) p q
  = FreshT
  $ \fun st
  -> do (res,st') <- runFreshT p fun st
        runFreshT (q res) fun st'

 return a = FreshT $ \_ st -> return (a, st)

-- | Generate a fresh name
-- A name may be given as a hint of what to prefix the new name with.
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

