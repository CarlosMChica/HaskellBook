{-# LANGUAGE InstanceSigs #-}

module ReaderInstances where

newtype Reader r a = Reader { runReader:: r -> a }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (\_ -> x)

  (<*>) :: Reader r (a -> b)
      ->  Reader r a
      ->  Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure

  (>>=) :: (Reader r a) -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb . ra $ r) r
