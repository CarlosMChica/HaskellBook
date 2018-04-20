{-# LANGUAGE Strict #-}
module StrictTest where

blah x = 1

main = print (blah undefined)

willForce x = 1

willNotForce ~x = 1

data List a = Cons a (List a)
            | Nil deriving Show

sTake :: Int -> List a -> List a
sTake n _ | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = Cons x (sTake (n - 1) xs)

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls
