module Semigroups where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

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

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

mainCombine = do
  print $ unCombine (f <> g ) 0
  print $ unCombine (f <> g ) 1
  print $ unCombine (f <> f ) 1
  print $ unCombine (g <> f ) 1

-- 10

newtype Comp a =
  Comp { unComp :: a -> a }

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return $ Comp f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genComp

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f ) <> (Comp g) = Comp $ f <> g

f1 = Comp $ \(Sum n) -> Sum (n + 1)
g1 = Comp $ \(Sum n) -> Sum (n - 1)

mainComp = do
  print $ unComp (f1 <> g1 )  0
  print $ unComp (f1 <> g1 )  1
  print $ unComp (f1 <> f1 )  1
  print $ unComp (g1 <> f1 )  1

-- 11

data Validation a b =
  Failuree a | Successs b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failuree x) <> (Failuree y) = Failuree $ x <> y
  (Failuree x) <> (Successs _) = Failuree x
  (Successs _) <> (Failuree y) = Failuree y
  (Successs x) <> (Successs _) = Successs x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Successs x, Failuree y]

type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

-- 12

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Successs x)) <> (AccumulateRight (Successs y)) = AccumulateRight $ Successs $ x <> y
  (AccumulateRight (Successs _)) <> (AccumulateRight (Failuree y)) = AccumulateRight $ Failuree y
  (AccumulateRight (Failuree x)) <> _                              = AccumulateRight $ Failuree x

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [AccumulateRight $ Successs x, AccumulateRight $ Failuree y]

type AccumulateRightAssoc = AccumulateRight Int String -> AccumulateRight Int String -> AccumulateRight Int String -> Bool

-- 13

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Successs x)) <> (AccumulateBoth (Successs y)) = AccumulateBoth $ Successs $ x <> y
  (AccumulateBoth (Failuree x)) <> (AccumulateBoth (Failuree y)) = AccumulateBoth $ Failuree $ x <> y
  (AccumulateBoth (Failuree x)) <> (AccumulateBoth (Successs _)) = AccumulateBoth $ Failuree x
  (AccumulateBoth (Successs _)) <> (AccumulateBoth (Failuree y)) = AccumulateBoth $ Failuree y

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [AccumulateBoth $ Successs x, AccumulateBoth $ Failuree y]

type AccumulateBothAssoc = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  -- quickCheck (semigroupAssoc :: CombineAssoc) Won't work without Eq instance for functions mehHH
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
