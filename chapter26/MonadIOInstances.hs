{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}

module MonadIOInstances where

import           Control.Applicative
import           Control.Monad.IO.Class

--1

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } deriving Functor

instance (Applicative f) => Applicative (MaybeT f) where
  pure :: a -> MaybeT f a
  pure x = MaybeT $ (pure . pure) x

  (<*>) :: MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
--  (MaybeT fmh) <*> (MaybeT fmx) = MaybeT $ liftA2 (<*>) fmh fmx
  (MaybeT fmh) <*> (MaybeT fmx) = MaybeT $ (<*>) <$> fmh <*> fmx

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT mma >>= amtb = MaybeT $ do
    ma <- mma
    case ma of
      Just x -> runMaybeT . amtb $ x
      _      -> return Nothing

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = MaybeT . fmap Just . liftIO

-- Example from page 1008
addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
  s1 <- sizeT f1
  s2 <- sizeT f2
  return (s1 + s2)

sizeT :: FilePath -> MaybeT IO a
sizeT = undefined

--2

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving Functor


instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure x = ReaderT $ pure <$> pure x

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ liftA2 (<*>) rmab rma

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= aRmb = ReaderT $ \r -> do
                                    a <- rma r
                                    runReaderT (aRmb a) r

instance MonadIO m => MonadIO (ReaderT e m) where
  liftIO :: IO a -> ReaderT e m a
  liftIO = ReaderT . const . liftIO
