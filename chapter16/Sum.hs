module Sum where

data Sum a b =
    First a
  | Second b

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second x) = Second $ f x
