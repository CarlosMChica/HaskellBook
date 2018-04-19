{-# LANGUAGE BangPatterns #-}

module StrictTest3 where

data List a = Cons !a (List a)
            | Nil deriving Show

sTake :: Int -> List a -> List a
sTake n _ | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x !xs) = Cons x (sTake (n - 1) xs)

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls

threeElements = Cons 2 twoEls
oneElT = sTake 1 threeElements

main = do
  print twoEls -- Blows up - Prints Cons 1 *** Exception
  print oneEl -- Blows up
  print threeElements -- Blows up - Prints Cons 2 Cons 1 *** Exception
  print oneElT -- Cons 2 Nil
