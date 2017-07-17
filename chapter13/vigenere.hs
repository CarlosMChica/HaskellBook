module Vigenere where

import Data.Char

caesar :: Int -> String -> String
caesar spaces = fmap (caesarChar spaces)

unCaesar :: String -> Int -> String
unCaesar []     _      = []
unCaesar (x:xs) spaces = shift x spaces (-) : unCaesar xs spaces

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

main = do
  putStr "Enter the message: "
  message <- getLine
  putStr "Enter the ciphering word: "
  word <- getLine
  putStrLn $ "Ciphered message: " ++ vigenere message word
--"MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
