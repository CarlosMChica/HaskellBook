module Main where

import Test.QuickCheck
import Data.List (sort)

-- 1
half x = x / 2

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll (arbitrary :: Gen Double)
  (\x -> x == ((*2) . half $ x))

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

sortedListGen :: Gen [Int]
sortedListGen = fmap sort (arbitrary :: Gen [Int])

prop_listOrdered :: Property
prop_listOrdered =
  forAll sortedListGen listOrdered

-- 3
prop_plusAssociative :: Property
prop_plusAssociative =
  forAll (arbitrary :: Gen Int)
  (\x y z -> x + (y + z) == (x + y) + z)

prop_plusCommutative:: Property
prop_plusCommutative =
  forAll (arbitrary :: Gen Double)
  (\x y -> x + y == y + x)

-- 4
prop_multAssociative :: Property
prop_multAssociative =
  forAll (arbitrary :: Gen Int)
  (\x y z -> x * (y * z) == (x * y) * z)

prop_multCommutative:: Property
prop_multCommutative =
  forAll (arbitrary :: Gen Double)
  (\x y -> x * y == y * x)

-- 5
prop_quotRem :: Property
prop_quotRem = undefined

prop_divMod :: Property
prop_divMod = undefined

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_multAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multCommutative
