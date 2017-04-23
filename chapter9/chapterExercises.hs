module ChapterExercises where

import Data.Char (toUpper, isUpper)

filterUpperCase :: String -> String
filterUpperCase []       = []
filterUpperCase (x : xs) = if isUpper x
                           then x : filterUpperCase xs
                           else filterUpperCase xs

capitaliseFirstLetter :: String -> String
capitaliseFirstLetter []       = []
capitaliseFirstLetter (x : xs) = toUpper x : xs

capitalise :: String -> String
capitalise []       = []
capitalise (x : xs) = toUpper x : capitalise xs

firstLetterUpperCase :: String -> Char
firstLetterUpperCase = toUpper . head

myOr :: [Bool] -> Bool
myOr []       = False
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []       = False
myAny f (x : xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _  []       = False
myElem x' (x : xs) = if x' == x then True else myElem x' xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []       = []
squish (x : xs) = x ++ squish xs

squishMaps :: (a -> [b]) -> [a] -> [b]
squishMaps _ []       = []
squishMaps f (x : xs) = f x ++ squishMaps f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMaps id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = go x xs
  where  go max [] = max
         go max (x : xs) = case f x max of
           GT -> go x xs
           _  -> go max xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = go x xs
  where  go max [] = max
         go max (x : xs) = case f x max of
           LT -> go x xs
           _  -> go max xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare


myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
