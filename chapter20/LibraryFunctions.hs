module LibraryFunctions where

import           Data.Foldable
import           Data.Monoid
import           Data.Semigroup
--1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

--2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

--3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . eq)
  where eq = (==) x

--4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldSemigroup min

--5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldSemigroup max

-- Couldn't make it work using the Semigroup typeclass and mappend
foldSemigroup :: Foldable t => (a -> a -> a) -> t a -> Maybe a
foldSemigroup f = foldr go Nothing
  where
    go x (Just x') = Just $ f x x'
    go x Nothing   = Just x

--6
null' :: Foldable t => t a -> Bool
null' = getAll . foldMap (\_ -> All False)

--7
length' :: Foldable t => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

--8
toList' :: Foldable t => t a -> [a]
toList' = foldMap (\x -> [x])

--9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

--10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x `mappend` acc) mempty
