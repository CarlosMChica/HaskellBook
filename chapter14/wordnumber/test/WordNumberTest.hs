module Main where

import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)

-- main :: IO ()
-- main = hspec $ do
--   describe "digitToWord" $ do
--     it "returns zero for 0" $
--       digitToWord 0 `shouldBe` "Zero"
--     it "returns one for 1" $
--       digitToWord 1 `shouldBe` "One"
-- 
--   describe "digits" $ do
--     it "returns [1] for 1" $
--       digits 1 `shouldBe` [1]
--     it "returns [1,0,0] for 100" $
--       digits 100 `shouldBe` [1, 0, 0]
--     it "returns [9,0,0,1] for 9001" $
--       digits 9001 `shouldBe` [9, 0, 0, 1]
-- 
--   describe "wordNumber" $ do
--     it "One-Zero-Zero given 100" $
--       wordNumber 100 `shouldBe` "One-Zero-Zero"
--     it "Five-Zero-One given 501" $
--       wordNumber 501 `shouldBe` "Five-Zero-One"
--     it "Nine-Zero-Zero-One for 9011" $
--       wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"

half x = x / 2

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll (arbitrary :: Gen Double)
  (\x -> x == ((*2) . half $ x))

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

prop_plusAssociative :: Property
prop_plusAssociative =
  forAll (arbitrary :: Gen Int)
  (\x y z -> x + (y + z) == (x + y) + z)

prop_plusCommutative:: Property
prop_plusCommutative =
  forAll (arbitrary :: Gen Double)
  (\x y -> x + y == y + x)

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
