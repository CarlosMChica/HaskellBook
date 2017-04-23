module ZippingExercises where

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (\x y -> (x, y))

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = go xs ys []
         where go [] _              acc = acc
               go _  []             acc = acc
               go (x : xs) (y : ys) acc = go xs ys (acc ++ [f x y])

