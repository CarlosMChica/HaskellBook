module Lib where

import Data.Char

caesar :: Int -> String -> String
caesar spaces = fmap (caesarChar spaces)

caesarChar :: Int -> Char -> Char
caesarChar = shift (+)

shift:: (Int -> Int -> Int) -> Int -> Char -> Char
shift f spaces x = chr $ (shifted `mod` alphabetLength) + firstLetterCode x
  where shifted = f (indexInAlphabet x) spaces
        alphabetLength = 26

vigenere :: String -> String -> String
vigenere xs word = go xs (cycle word)
  where  go [] _              = ""
         go (' ' : xs) word   = ' ' : go xs word
         go (x : xs) (y : ys) = caesarChar (indexInAlphabet y) x : go xs ys

firstLetterCode :: Char -> Int
firstLetterCode c
  | isUpper c = ord 'A'
  | isLower c = ord 'a'

indexInAlphabet :: Char -> Int
indexInAlphabet c = ord c - firstLetterCode c
