module Main where

import Lib

main = do
  putStr "Enter the message: "
  message <- getLine
  putStr "Enter the ciphering word: "
  word <- getLine
  putStrLn $ "Ciphered message: " ++ vigenere message word
--"MEET AT DAWN" "ALLY" == "MPPR AE OYWY"
