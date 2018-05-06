{-# LANGUAGE BangPatterns #-}

module StrictTest4 where

data List a = Nil | Cons !a !(List a) deriving Show

sTake :: Int -> List a -> List a
sTake n _ | n <= 0 = Nil
stake n Nil = Nil
stake n (Cons x xs) = (Cons x (stake (n - 1) xs))

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls

main :: IO ()
main = do
  print twoEls -- Blows up
  print oneEl -- Blows up
