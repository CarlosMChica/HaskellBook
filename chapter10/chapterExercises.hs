module ChapterExercises where

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(s, v, s') | s <- stops , v <- vowels , s' <- stops]

stopVowelStop' :: [(Char, Char, Char)]
stopVowelStop' = [(s, v, s') | s <- stops , v <- vowels , s' <- stops , s == 'p']

nouns = ["house", "table", "lamp"]
verbs = ["open", "buy", "move"]

nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(n, v, n') | n <- nouns, v <- verbs, n' <- nouns]

seekritFunc :: Fractional a => String -> a
seekritFunc x = (/) (fromIntegral (sum (map length (words x))))
                    (fromIntegral (length (words x )))

myOr :: [Bool] -> Bool
myOr = or

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myElem :: Eq a => a -> [a] -> Bool
myElem a' = foldr (\a b -> a == a' || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a' = myAny (a' ==)

myReverse :: [a] -> [a]
myReverse = foldr (\a b -> b ++ [a]) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . foldr (\a b -> f a : b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = myOrderingBy f GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myOrderingBy f LT

myOrderingBy :: (a -> a -> Ordering) -> Ordering-> [a] -> a
myOrderingBy f ord xs = foldl (\b a -> if f b a == ord then b else a) (head xs) xs 
