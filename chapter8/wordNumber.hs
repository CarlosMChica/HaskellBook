module WordNumber where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n = case n of
                     0 -> "Zero"
                     1 -> "One"
                     2 -> "Two"
                     3 -> "Three"
                     4 -> "Four"
                     5 -> "Five"
                     6 -> "Six"
                     7 -> "Seven"

digits :: Int -> [Int]
digits n = go n []
     where go n digits
            | n == 0    = digits
            | otherwise =  go (n `div` 10) ((n `mod` 10) : digits)

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
