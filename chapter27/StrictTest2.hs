module StrictTest2 where

data List a = Cons a! (List a)
            | Nil deriving Show

sTake :: Int -> List a -> List a
sTake n _ | n <= 0 = Nil
sTake n Nil = Nil
sTake n (Cons x xs) = Cons x (sTake (n - 1) xs)

twoEls = Cons 1 (Cons undefined Nil)
oneEl = sTake 1 twoEls

main = do
  print twoEls -- blows up
  print oneEl -- Prints Cons 1 Nill. It does not blow up as it only evaluates the first head which is 1
