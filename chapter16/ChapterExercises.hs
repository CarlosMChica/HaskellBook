module ChapterExercises where

import GHC.Arr
import FunctorLaws
import Test.QuickCheck
import Test.QuickCheck.Function

-- 1.

-- data Bool = False | True
-- No valid Functor instance possible as Bool is not a higher kinded type, it's kind is *, functor needs * -> *

-- 2.

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' $ f x
  fmap f (True' x)  = True' $ f x

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    oneof [return $ False' x, return $ True' x]

type BoolAndSomethingElseId = BoolAndSomethingElse String -> Bool
type BoolAndSomethingElseCompose = Fun String Int -> Fun Int String -> BoolAndSomethingElse String -> Bool

main = do
  quickCheck (functorIdentity :: BoolAndSomethingElseId)
  quickCheck (functorCompose :: BoolAndSomethingElseCompose)

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- This is equivalent to Maybe and I'm lazy so I'm not writing it again :). It's a perfectly valid functor.

-- 4.

newtype Mu f = Inf { outF :: f (Mu f) }

-- Not Possible as Mu has kind (* -> *) -> * and Functor requires kind * -> *.
-- (* -> *) -> * This kind means that the type argument f is itself a higher-kinded type. Applying f would leave kind *.

-- 5.

data D = D (Array Word Word) Int Int

-- No valid Functor instance possible as Bool is not a higher kinded type, it's kind is *, functor needs * -> *

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1.

-- Given type
-- data Sum a b = First a | Second b

-- Fixed type
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b
