module GivenATypeWriteTheFunction where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a ->  b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r = tail
-- solution 2 r (x:xs) = [x]
-- soltuion 3 r xs = reverse xs

co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f = f
