{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module River.Fresh (
    MonadFresh(..)

  , Fresh
  , runFresh
  , runFreshFrom

  , FreshT
  , runFreshT
  , runFreshFromT

  , FreshName(..)
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Cont (ContT)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.List (ListT)
import           Control.Monad.Trans.Maybe (MaybeT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import           Control.Monad.Trans.State.Strict (evalStateT, get, put)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Functor.Identity (Identity, runIdentity)

import           River.Name

------------------------------------------------------------------------

type Fresh =
  FreshT Identity

newtype FreshT m a =
  FreshT {
      unFreshT :: Strict.StateT FreshContext m a
    } deriving (Functor, Applicative, Monad)

newtype FreshContext =
  FreshContext {
      _freshSupply :: [Int]
    }

------------------------------------------------------------------------

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

------------------------------------------------------------------------

class FreshName n where
  nextName :: n -> Int
  freshen :: MonadFresh m => n -> m n
  newFresh :: MonadFresh m => m n

instance FreshName (Name n) where
  nextName = \case
    Name _ ->
      1
    NameMod _ n ->
      n + 1
    NameNew n ->
      n + 1

  freshen = \case
    Name n ->
      NameMod n <$> nextFresh
    NameMod n _ ->
      NameMod n <$> nextFresh
    NameNew _ ->
      newFresh

  newFresh =
    NameNew <$> nextFresh

------------------------------------------------------------------------

class Monad m => MonadFresh m where
  nextFresh :: m Int

instance Monad m => MonadFresh (FreshT m) where
  nextFresh =
    FreshT $ do
      FreshContext (x : xs) <- get
      put $ FreshContext xs
      pure x

instance MonadFresh m => MonadFresh (ContT r m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (ExceptT e m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (IdentityT m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (ListT m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (MaybeT m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (ReaderT r m) where
  nextFresh =
    lift nextFresh

instance (MonadFresh m, Monoid w) => MonadFresh (Lazy.WriterT w m) where
  nextFresh =
    lift nextFresh

instance (MonadFresh m, Monoid w) => MonadFresh (Strict.WriterT w m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (Lazy.StateT s m) where
  nextFresh =
    lift nextFresh

instance MonadFresh m => MonadFresh (Strict.StateT s m) where
  nextFresh =
    lift nextFresh

instance (MonadFresh m, Monoid w) => MonadFresh (Lazy.RWST r w s m) where
  nextFresh =
    lift nextFresh

instance (MonadFresh m, Monoid w) => MonadFresh (Strict.RWST r w s m) where
  nextFresh =
    lift nextFresh
