module AsPatterns where

import Data.Char

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf t@(x : xs) (y : ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf t ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = fmap (\word@(y : ys) -> (word, toUpper y : ys)) . words
