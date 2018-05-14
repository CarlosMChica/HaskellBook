#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.5
  --package criterion
-}
module ChapterExercises where

import Criterion.Main

newtype DList a = DL { unDl :: [a] -> [a] }

--1
empty :: DList a
empty = DL id
{-# INLINE empty #-}

--2
singleton :: a -> DList a
singleton x = DL (x:)
{-# INLINE singleton #-}
--3
toList :: DList a -> [a]
toList xs = unDl xs []
{-# INLINE toList #-}
--4
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = singleton x `append` xs
{-# INLINE cons #-}

--5
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = xs `append` singleton x
{-# INLINE snoc #-}

--6
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDl xs . unDl ys
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n-1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
  [ bench "concat list" $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]

data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x q = q { enqueue = x : enqueue q}

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop q = case dequeue q of
    []     -> let (x : xs) = reverse . enqueue $ q in Just (x, Queue [] xs)
    (x:xs) -> Just (x, Queue [] $ xs)
