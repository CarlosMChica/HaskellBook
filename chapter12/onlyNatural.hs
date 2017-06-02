module OnlyNatural where

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

-- We are really folding the Nat type into an integer
natToInteger :: Nat -> Integer
natToInteger x = go x 0
  where go Zero     count = count
        go (Succ x) count = go x (count + 1)

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0     = Nothing
  | otherwise = Just $ toNat x
  where toNat 0 = Zero
        toNat x = Succ $ toNat (x - 1)
