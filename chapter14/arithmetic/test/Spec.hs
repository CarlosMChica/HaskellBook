module Main where

import Test.QuickCheck
import Test.QuickCheck.Function (apply, Fun(..))
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

prop_quotRem :: NonZero Int -> NonZero Int -> Bool
prop_quotRem (NonZero x) (NonZero y) =
  (quot x y) * y + (rem x y) == x

prop_divMod :: NonZero Int -> NonZero Int -> Bool
prop_divMod (NonZero x) (NonZero y) =
  (div x y) * y + (mod x y) == x

-- 6
prop_powerNotAssociative :: Property
prop_powerNotAssociative =
  forAll (arbitrary :: Gen (Integer, Integer, Integer))
  (\(x, y, z) -> (x ^ y) ^ z == x ^ (y ^ z))

prop_powerNotCommutative :: Int -> Int -> Bool
prop_powerNotCommutative x y = x ^ y == y ^ x

-- 7
prop_reverse :: [String] -> Bool
prop_reverse xs = (reverse . reverse) xs == id xs

-- 8
prop_apply :: Fun Int Int -> Int -> Bool
prop_apply (Fun _ f) a = (f $ a) == (f a)

prop_functionComposition:: Fun String Int -> Fun Double String -> Double -> Bool
prop_functionComposition (Fun _ f) (Fun _ g) a = (f . g $ a) == f (g a)
-- alternatively with apply :: Fun a b -> a -> b
-- prop_functionComposition f g x = ((apply f) . (apply g)) x == (apply f) ((apply g) x)

-- 9
prop_foldRConsNotEqualsAppend :: Property
prop_foldRConsNotEqualsAppend =
  forAll (arbitrary :: Gen [Integer])
  (\x y -> foldr (:) x y == (++) y x)

prop_foldRConcatEmptyAccEqualsToConcat :: [String] -> Bool
prop_foldRConcatEmptyAccEqualsToConcat xs = foldr (++) [] xs == concat xs

-- 10
prop_takeLengthNotEqualsN :: Int -> [String] -> Bool
prop_takeLengthNotEqualsN n xs = length (take n xs) == n

-- 11
prop_readShow :: String -> Bool
prop_readShow x = (read . show) x == x

main :: IO ()
main = do
  putStrLn ""
  putStrLn "half identity"
  quickCheck prop_halfIdentity
  putStrLn "sort"
  quickCheck prop_listOrdered
  putStrLn "Addition is associative"
  quickCheck prop_plusAssociative
  putStrLn "Multiplication is associative"
  quickCheck prop_multAssociative
  putStrLn "Addition is commutative"
  quickCheck prop_plusCommutative
  putStrLn "Multiplication is commutative"
  quickCheck prop_multCommutative
  putStrLn "QuotRem"
  quickCheck prop_quotRem
  putStrLn "DivMod"
  quickCheck prop_divMod
  putStrLn "Power Not associative"
  quickCheck prop_powerNotAssociative
  putStrLn "Power Not commutative"
  quickCheck prop_powerNotCommutative
  putStrLn "Reversing twice is equals to id"
  quickCheck prop_reverse
  putStrLn "Apply"
  quickCheck prop_apply
  putStrLn "Function composition"
  quickCheck prop_functionComposition
  putStrLn "foldRConsNotEqualsAppend"
  quickCheck prop_foldRConsNotEqualsAppend
  putStrLn "foldRConcatEmptyAccEqualsToConcat"
  quickCheck prop_foldRConcatEmptyAccEqualsToConcat
  putStrLn "takeLengthNotEqualsN"
  quickCheck prop_takeLengthNotEqualsN
  putStrLn "readShow"
  quickCheck prop_readShow
