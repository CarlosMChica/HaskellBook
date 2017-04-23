{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
  tooMany :: a -> Bool

instance (Ord a, Num a) => TooMany (a, String)  where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int)  where
  tooMany (a1, a2) = a1 + a2 > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a1, a2) = tooMany (a1 + a2)
