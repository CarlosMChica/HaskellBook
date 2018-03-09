{-# LANGUAGE InstanceSigs #-}

module MonadTrans where

import           Control.Monad.Trans

--1

newtype Identity a = Identity a
newtype IdentityT m a = IdentityT (m (Identity a))

instance MonadTrans IdentityT where
  lift :: Functor m => m a -> IdentityT m a
  lift = IdentityT . fmap Identity

--2
newtype MaybeT m a = MaybeT (m (Maybe a))

instance MonadTrans MaybeT where
  lift :: Functor m => m a -> MaybeT m a
  lift = MaybeT . fmap Just

--3
newtype ReaderT r m a = ReaderT (r -> m a)

instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift = ReaderT . const
