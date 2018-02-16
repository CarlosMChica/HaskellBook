{-# LANGUAGE InstanceSigs #-}

module TwinApplicative where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ (pure . pure) x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose fgab) <*> (Compose fga) = Compose $ (fmap (<*>) fgab) <*> fga
