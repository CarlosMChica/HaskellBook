module Reverse where

import Data.List.Split

rvrs :: String -> String
rvrs x = three ++ " " ++ two ++ " " ++ one
  where parts = splitOn " "  x
        one   = head parts
        two   = parts !! 1
        three = parts !! 2

main :: IO ()
main = print $ rvrs "Curry is awesome"
