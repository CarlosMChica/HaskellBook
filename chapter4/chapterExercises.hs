module ChapterExercises where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a, b) -> (c, d) -> ((b, d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)

ftwo    xs = x w 1
  where w  = length xs
