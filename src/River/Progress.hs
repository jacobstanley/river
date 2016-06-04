{-# LANGUAGE LambdaCase #-}
module River.Progress (
    MonadProgress(..)

  , Progress
  , runProgress
  , fixProgress

  , ProgressT
  , runProgressT
  , fixProgressT
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
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict

import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Monoid (Monoid(..), (<>))

------------------------------------------------------------------------

type Progress =
  ProgressT Identity

newtype ProgressT m a =
  ProgressT {
      unProgressT :: m (Progression, a)
    }

data Progression =
   Progress
 | NoProgress
   deriving (Eq, Ord, Show)

------------------------------------------------------------------------

runProgress :: Progress a -> a
runProgress =
  runIdentity . runProgressT

runProgressT :: Monad m => ProgressT m a -> m a
runProgressT m = do
  (_, y) <- unProgressT m
  pure y

fixProgress :: (a -> Progress a) -> a -> a
fixProgress f x =
  runIdentity $ fixProgressT f x

fixProgressT :: Monad m => (a -> ProgressT m a) -> a -> m a
fixProgressT f x = do
  (p, y) <- unProgressT (f x)
  case p of
    Progress ->
      fixProgressT f y
    NoProgress ->
      pure y

------------------------------------------------------------------------

instance Monoid Progression where
  mempty =
    NoProgress

  mappend x y =
    case (x, y) of
      (Progress, _) ->
        Progress
      (_, Progress) ->
        Progress
      (NoProgress, NoProgress) ->
        NoProgress

instance Functor m => Functor (ProgressT m) where
  fmap f =
    ProgressT . fmap (fmap f) . unProgressT

instance Applicative m => Applicative (ProgressT m) where
  pure x =
    ProgressT $
      pure (NoProgress, x)

  pmf <*> pmx =
    let
      go (pf, f) (px, x) =
        (pf <> px, f x)
    in
      ProgressT $
        fmap go (unProgressT pmf) <*> (unProgressT pmx)

instance Monad m => Monad (ProgressT m) where
  return =
    pure

  pmx >>= pmf =
    ProgressT $ do
      (px, x) <- unProgressT pmx
      (py, y) <- unProgressT $ pmf x
      pure (px <> py, y)

------------------------------------------------------------------------

class Monad m => MonadProgress m where
  progress :: a -> m a

instance Monad m => MonadProgress (ProgressT m) where
  progress x =
    ProgressT $
      pure (Progress, x)

instance MonadProgress m => MonadProgress (ContT r m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (ExceptT e m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (IdentityT m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (ListT m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (MaybeT m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (ReaderT r m) where
  progress =
    lift . progress

instance (MonadProgress m, Monoid w) => MonadProgress (Lazy.WriterT w m) where
  progress =
    lift . progress

instance (MonadProgress m, Monoid w) => MonadProgress (Strict.WriterT w m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (Lazy.StateT s m) where
  progress =
    lift . progress

instance MonadProgress m => MonadProgress (Strict.StateT s m) where
  progress =
    lift . progress

instance (MonadProgress m, Monoid w) => MonadProgress (Lazy.RWST r w s m) where
  progress =
    lift . progress

instance (MonadProgress m, Monoid w) => MonadProgress (Strict.RWST r w s m) where
  progress =
    lift . progress
