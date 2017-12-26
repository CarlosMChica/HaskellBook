module FizzBuzz where

import           Control.Monad
import           Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz x
 | x `mod` 15 == 0  = "FizzBuzz"
 | x `mod` 5 == 0   = "Buzz"
 | x `mod` 3 == 0   = "Fizz"
 | otherwise       = show x

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzbuzzList $ enumFromThenTo to (pred to) from
