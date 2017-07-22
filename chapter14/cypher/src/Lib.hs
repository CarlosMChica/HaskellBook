module Lib
(caesar, unCaesar, vigenere, unVigenere,
 Key, PlainText, EncrypedText, Spaces)
where

import Data.Char
import Control.Applicative()

type Key = String
type PlainText = String
type EncrypedText = String
type Spaces = Int

caesar :: PlainText -> Spaces -> EncrypedText
caesar text spaces = caesarChar spaces <$> text

unCaesar :: EncrypedText -> Spaces -> PlainText
unCaesar []     _      = []
unCaesar (x:xs) spaces = unCaesarChar spaces x : unCaesar xs spaces

vigenere :: PlainText -> Key -> EncrypedText
vigenere = vigenere' caesarChar

unVigenere :: EncrypedText -> Key -> PlainText
unVigenere = vigenere' unCaesarChar

vigenere' :: (Spaces -> Char -> Char) -> String -> Key -> String
vigenere' f xs key = go xs (cycle key)
  where  go [] _              = ""
         go (' ' : xs) key    = ' ' : go xs key
         go (x : xs) (y : ys) = f (indexInAlphabet y) x : go xs ys

caesarChar :: Int -> Char -> Char
caesarChar = shift (+)

unCaesarChar :: Int -> Char -> Char
unCaesarChar = shift (-)

shift:: (Int -> Int -> Int) -> Int -> Char -> Char
shift f spaces x = chr $ (shifted `mod` alphabetLength) + firstLetterCode x
  where shifted = f (indexInAlphabet x) spaces
        alphabetLength = 26

firstLetterCode :: Char -> Int
firstLetterCode c
  | isUpper c = ord 'A'
  | isLower c = ord 'a'

indexInAlphabet :: Char -> Int
indexInAlphabet c = ord c - firstLetterCode c
