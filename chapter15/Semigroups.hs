module Semigroups where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.Semigroup (Semigroup, (<>))

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c))  == ((a <> b) <> c)

-- 1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (Identity x ) <> (Identity y) = Identity (x <> y)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return (Identity x)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Two x y)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 6

newtype BoolConj =
  BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y)  = BoolConj (x && y)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 8

data Or a b =
   Fst a
 | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd x) <> _       = Snd x
  _       <> y       = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Fst x, Snd y]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

-- 9

newtype Combine a b =
  Combine { unCombine :: a -> b }

genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
genFunc = arbitrary

genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
  f <- genFunc
  return $ Combine f

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = genCombine

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

type CombineAssoc = Combine Int String -> Combine Int String -> Combine Int String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  -- quickCheck (semigroupAssoc :: CombineAssoc) Won't work without Eq instance for functions mehHH
