module Possibly where

data Possibly a =
    LolNope
  | Yeppers a

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers x) = Yeppers $ f x
