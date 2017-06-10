module Palindrome where

import Data.Char (toLower, isAlpha)
import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case line1 == reverse line1 of
    True -> putStrLn "It's a palindrome"
    False -> do
               putStrLn "Nope!"
               exitSuccess

palindrome' :: IO ()
palindrome' = forever $ do
  line <- getLine
  if isPalindrome line then 
    putStrLn "It's a palindrome"
  else
    do  putStrLn "Nope!"
        exitSuccess

isPalindrome :: String -> Bool
isPalindrome [] = False
isPalindrome xs =
  let half = splitInHalf sentence
  in  fst half == (reverse . snd $ half)
     where sentence = filter isAlpha . fmap toLower $ xs
           splitInHalf xs =
             let half = splitAt (length xs `div` 2) xs
             in if even . length $ sentence then
                   half
                 else
                   (fst half, drop 1 . snd $ half)

