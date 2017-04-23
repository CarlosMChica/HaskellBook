module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar []     _      = []
caesar (x:xs) spaces = shift x spaces (+) : caesar xs spaces


unCaesar :: String -> Int -> String
unCaesar []     _      = []
unCaesar (x:xs) spaces = shift x spaces (-) : unCaesar xs spaces

shift:: Char -> Int -> (Int -> Int -> Int) -> Char
shift x spaces f = chr $ (shifted `mod` alphabetLength) + base
  where shifted = f (ord x - base) spaces
        base = ord 'a'
        alphabetLength = 26
