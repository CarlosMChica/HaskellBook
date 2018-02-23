{-# LANGUAGE InstanceSigs #-}

module MaybeT where

import           Control.Applicative

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

instance (Monad f) => Monad (MaybeT f) where
  return :: a -> MaybeT f a
  return = pure

  (>>=) :: MaybeT f a -> (a -> MaybeT f b) -> MaybeT f b
  MaybeT fma >>= amtb = MaybeT $ do
    ma <- fma
    case ma of
      Just x    -> runMaybeT . amtb $ x
      otherwise -> return Nothing
