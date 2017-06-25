module Main where

import Test.Hspec
import Test.QuickCheck
import WordNumber (digitToWord, digits, wordNumber)
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $
      digitToWord 0 `shouldBe` "Zero"
    it "returns one for 1" $
      digitToWord 1 `shouldBe` "One"

  describe "digits" $ do
    it "returns [1] for 1" $
      digits 1 `shouldBe` [1]
    it "returns [1,0,0] for 100" $
      digits 100 `shouldBe` [1, 0, 0]
    it "returns [9,0,0,1] for 9001" $
      digits 9001 `shouldBe` [9, 0, 0, 1]

  describe "wordNumber" $ do
    it "One-Zero-Zero given 100" $
      wordNumber 100 `shouldBe` "One-Zero-Zero"
    it "Five-Zero-One given 501" $
      wordNumber 501 `shouldBe` "Five-Zero-One"
    it "Nine-Zero-Zero-One for 9011" $
      wordNumber 9001 `shouldBe` "Nine-Zero-Zero-One"
