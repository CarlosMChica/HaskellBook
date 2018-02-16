{-# LANGUAGE InstanceSigs #-}

module Bifunctor where

class Bifunctor f where
  {-# MINIMAL bimap | first, second #-}

  bimap ::  (a -> b) -> (c -> d) -> f a c -> f b d
  bimap f g = (first f . second g)

  first :: (a -> b) -> f a c -> f b c
  first f = bimap f id

  second :: (c -> d) -> f a c -> f a d
  second f = second f


data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const x) = Const $ f x

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

data Quadricepts a b c d = Quadricepts a b c d

instance Bifunctor (Quadricepts a b) where
  bimap f g (Quadricepts x y z t) = Quadricepts x y (f z) (g t)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap f g either = case either of
    Left'  x -> Left' $ f x
    Right' y -> Right' $ g y
