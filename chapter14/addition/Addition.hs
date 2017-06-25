module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

multiply :: (Eq a, Num a) => a -> a -> a
multiply multiplicand multiplier = go (abs multiplicand) (abs multiplier) 0
  where go _ 0 acc = acc * (signum multiplicand * signum multiplier)
        go x y acc = go x (y - 1) (acc + x)

main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "5 multiplied by 3 is 15" $ do
      multiply 5 3 `shouldBe` 15
    it "0 multiplied by 10 is 0" $ do
      multiply 0 15 `shouldBe` 0
    it "5 multiplied by 0 is 0" $ do
      multiply 5 0 `shouldBe` 0
    it "5 multiplied by -1 is -5" $ do
      multiply 5 (-1) `shouldBe` (-5)
    it "-1 multiplied by 5 is -5" $ do
      multiply (-1) 5 `shouldBe` (-5)
    it "-1 multiplied by -5 is 5" $ do
      multiply (-1) (-5) `shouldBe` (5)
    it "x + 1 always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']
