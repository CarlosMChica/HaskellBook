module EqInstances where

-- exercise 1

data TisAndInteger = ThisAn Integer

instance Eq TisAndInteger where
  (==) (ThisAn x) (ThisAn x') =
    x == x'

-- exercise 2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') =
    x == x' && y == y'

-- exercise 3

data StringOrInt = TisAndInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAndInt x) (TisAndInt x')   = x == x'
  (==) (TisAString x) (TisAString x') = x == x'
  (==) _ _                            = False

-- exercise 4

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

-- exercise 5

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- exercise 6

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _                      = False

-- exercise 7

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a')     = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _                      = False
