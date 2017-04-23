module ChapterExercises where

sumRecursive :: (Eq a, Num a) => a -> a
sumRecursive x = go x 0
 where go x count
        | x == 0 = count
        | otherwise = go (x - 1) (count + x)

multRecursive :: (Integral a) => a -> a -> a
multRecursive multiplicand multiplier = go (abs multiplicand) (abs multiplier) 0
  where go x y acc
         | y == 0 = acc * (signum multiplicand * signum multiplier)
         | otherwise = go x (y - 1) (acc + x)

divideBy :: (Integral a ) => a -> a -> Maybe (a, a)
divideBy num 0 = Nothing
divideBy num den = Just (go (abs num) (abs den) 0 (signum num * signum den))
  where go n d acc s
         | n < d = (acc * s, n * s)
         | otherwise = go (n - d) d (acc + 1) s

mc91 :: (Integral a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11
