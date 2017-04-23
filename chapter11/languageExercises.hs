module LanguageExercises where

import Data.Char
import Data.List.Split
import Data.List

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs)
  | isAlpha x = toUpper x : xs
  | otherwise = x : capitalizeWord xs

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate "." . fmap capitalizeWord . splitOn "."
