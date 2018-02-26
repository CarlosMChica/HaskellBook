{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor f) => Functor (MaybeT f) where
  fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
  fmap h (MaybeT fma) = MaybeT $ (fmap . fmap) h fma

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
      Just x    -> runMaybeT . amtb $ x
      _         -> return Nothing
