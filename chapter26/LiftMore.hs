{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module LiftMore where

import           Control.Monad.Trans

--1

newtype EitherT e m a = EitherT (m (Either e a))

instance MonadTrans (EitherT e) where
  lift :: Functor m => m a -> EitherT e m a
  lift = EitherT . fmap Right

--2

newtype StateT s m a = StateT (s -> m (a, s))

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
