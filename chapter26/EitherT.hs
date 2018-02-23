{-# LANGUAGE InstanceSigs #-}

module EitherT where

import           Control.Applicative

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

--1.
instance Functor f => Functor (EitherT e f) where
  fmap :: (a -> b) -> EitherT e f a -> EitherT e f b
  fmap ab (EitherT fea) = EitherT $ (fmap . fmap) ab fea

--2.
instance Applicative f => Applicative (EitherT e f) where
  pure :: a -> EitherT e f a
  pure x = EitherT $ (pure . pure) x

  (<*>) :: EitherT e f (a -> b) -> EitherT e f a -> EitherT e f b
--  (EitherT meab) <*> (EitherT mea) = EitherT $ (<*>) <$> meab <*> mea
  (EitherT meab) <*> (EitherT mea) = EitherT $ liftA2 (<*>) meab mea

--3.
instance Monad f => Monad (EitherT e f) where
  return :: a -> EitherT e f a
  return = pure

  (>>=) :: EitherT e f a -> (a -> EitherT e f b) -> EitherT e f b
  (EitherT fea) >>= afeb = EitherT $ do
    ea <- fea
    case ea of
      Right x -> runEitherT . afeb $ x
      Left y  -> return . Left $ y

--4.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swap <$> mea
  where swap :: Either a b -> Either b a
        swap (Left x)  = Right x
        swap (Right y) = Left y

--5.
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT meb) = do
  eb <- meb
  case eb of
    Right x -> g x
    Left y  -> f y
