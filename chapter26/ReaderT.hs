{-# LANGUAGE InstanceSigs #-}

module ReaderT where

import Control.Applicative

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor f => Functor (ReaderT r f) where
  fmap :: (a -> b) -> ReaderT r f a -> ReaderT r f b
  --fmap h (ReaderT rfa) = ReaderT $ \r -> h <$> rfa r
  fmap h (ReaderT rfa) = ReaderT $ (fmap . fmap) h rfa

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  --pure x = ReaderT $ \_ -> pure x
  --pure x = ReaderT $ (const (pure x))
  --pure x = ReaderT $ (pure . pure $ x)
  --pure x = ReaderT $ pure <$> const x
  pure x = ReaderT $ pure <$> pure x

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  --(ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (\r -> (rmab r) <*> (rma r))
  --(ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) rmab rma

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  --(ReaderT rma) >>= f = ReaderT (\r -> rma r >>= (\x -> runReaderT (f x) $ r))
  (ReaderT rma) >>= aRmb = ReaderT $ \r -> do
                                    a <- rma r
                                    runReaderT (aRmb a) r
