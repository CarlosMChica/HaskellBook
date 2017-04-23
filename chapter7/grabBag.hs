module GrabBag where

-- exercise 3
-- a
addOneIfOdd :: Integral a => a -> a
addOneIfOdd = \x -> if odd x then f x else x
  where f = (+ 1)

-- b
addFive :: (Ord a, Num a) => a -> a -> a
addFive = \x y -> if x > y then y else x + 5

-- c
mFlip f x y = f y x
