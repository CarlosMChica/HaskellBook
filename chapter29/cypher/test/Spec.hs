module Main where

import Test.QuickCheck
import Lib

genNonEmptyAlphaString :: Gen PlainText
genNonEmptyAlphaString = listOf1 genAlphaChar
  where genAlphaChar :: Gen Char
        genAlphaChar = elements ['a'..'z']

genCaesar :: Gen (PlainText, Int)
genCaesar = do
  plainText <- genNonEmptyAlphaString
  spaces <- arbitrary
  return (plainText, spaces)

genVigenere:: Gen (PlainText, Key)
genVigenere = do
  plainText <- genNonEmptyAlphaString
  key <- genNonEmptyAlphaString
  return (plainText, key)

prop_encode_decode_caesar :: Property
prop_encode_decode_caesar =
  forAll genCaesar
  (\(plainText, spaces)-> unCaesar (caesar plainText spaces) spaces == plainText)

prop_encode_decode_vigenere:: Property
prop_encode_decode_vigenere =
  forAll genVigenere
  (\(plainText, key)-> unVigenere (vigenere plainText key) key == plainText)

main :: IO ()
main = do
  putStrLn ""
  putStrLn "Encode-Decode caesar"
  quickCheck prop_encode_decode_caesar
  putStrLn "Encode-Decode vigenere"
  quickCheck prop_encode_decode_vigenere
