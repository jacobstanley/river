{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module River.Fresh (
    Fresh
  , runFresh
  , runFreshFrom

  , FreshT
  , runFreshT
  , runFreshFromT

  , nextFresh
  , freshen
  , newFresh
  ) where

import           Control.Monad.Trans.State (StateT, evalStateT, get, put)

import           Data.Functor.Identity (Identity, runIdentity)

import           River.Name


type Fresh =
  FreshT Identity

newtype FreshT m a =
  FreshT {
      unFreshT :: StateT FreshContext m a
    } deriving (Functor, Applicative, Monad)

newtype FreshContext =
  FreshContext {
      _freshSupply :: [Int]
    }

runFresh :: Fresh a -> a
runFresh =
  runIdentity . runFreshT

runFreshFrom :: Int -> Fresh a -> a
runFreshFrom n =
  runIdentity . runFreshFromT n

runFreshT :: Monad m => FreshT m a -> m a
runFreshT =
  runFreshFromT 1

runFreshFromT :: Monad m => Int -> FreshT m a -> m a
runFreshFromT n =
  flip evalStateT (FreshContext [n..]) . unFreshT

nextFresh :: Monad m => FreshT m Int
nextFresh =
  FreshT $ do
    FreshContext (x : xs) <- get
    put $ FreshContext xs
    pure x

freshen :: Monad m => Name n -> FreshT m (Name n)
freshen = \case
  Name n ->
    NameMod n <$> nextFresh
  NameMod n _ ->
    NameMod n <$> nextFresh
  NameNew _ ->
    newFresh

newFresh :: Monad m => FreshT m (Name n)
newFresh =
  NameNew <$> nextFresh
