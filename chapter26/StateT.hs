{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor f => Functor (StateT s f) where
  fmap :: (a -> b) -> StateT s f a -> StateT s f b
  fmap h (StateT smas) = StateT $ \s -> mapFst h <$> smas s
    where mapFst :: (a -> b) -> (a, c) -> (b, c)
          mapFst f (x, y) = (f x, y)

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s ->
    -- sma s >>= (\(x, s') -> (\(f, s'') -> (f x, s')) <$> smab s)
    do
      (f, s') <- smab s
      (x, s'') <- sma s'
      return (f x, s'')

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  --(StateT sma) >>= f = StateT $ \s -> sma s >>= (\(a, s') -> runStateT (f a) s')
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'
