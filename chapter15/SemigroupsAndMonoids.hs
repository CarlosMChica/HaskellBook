module SemigroupsAndMonoids where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Monad
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))
import MonoidProps

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c))  == ((a <> b) <> c)

-- SEMIGROUPS

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


-- 10

newtype Comp a =
  Comp { unComp :: a -> a }

genComp :: (CoArbitrary a, Arbitrary a) => Gen (Comp a)
genComp = do
  f <- genFunc
  return $ Comp f

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = genComp

instance Semigroup (Comp a) where
  (Comp f ) <> (Comp g) = Comp $ f . g

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

-- MONOIDS

-- 1
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

-- 2
instance (Monoid a, Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- 3

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- 4

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

-- 6

instance (Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine $ const mempty
  mappend = (<>)

-- 7

instance (Semigroup a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- 8

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ \s -> let (a, s') = f s
                                       (a', s'') = g s'
                                       in (a <> a', s'')

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  mappend = (<>)

-- MAIN

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

mainCombine = do
  print $ unCombine (f <> g ) 0
  print $ unCombine (f <> g ) 1
  print $ unCombine (f <> f ) 1
  print $ unCombine (g <> f ) 1
  print $ unCombine (f `mappend` mempty) 1
  print $ unCombine (mempty `mappend` f) 1

f1 = Comp $ \(Sum n) -> Sum (n + 1)
g1 = Comp $ \(Sum n) -> Sum (n - 1)

mainComp = do
  print $ unComp (f1 <> g1 )  0
  print $ unComp (f1 <> g1 )  1
  print $ unComp (f1 <> f1 )  1
  print $ unComp (g1 <> f1 )  1
  print $ unComp f1 1
  print $ unComp (f1 `mappend` mempty) 1
  print $ unComp (mempty `mappend` f1) 1

f2 :: Mem Integer String
f2 = Mem $ \s -> ("hi", s + 1)

mainMem = do
  print $ runMem (f2 <> mempty) 0
  print $ runMem (mempty <> f2) 0
  print (runMem mempty 0 :: (String, Int))
  print $ runMem (f2 <> mempty) 0 == runMem f2 0
  print $ runMem (mempty <> f2) 0 == runMem f2 0

main = do
  putStrLn "Checking semigroups"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  -- quickCheck (semigroupAssoc :: CombineAssoc) Won't work without Eq instance for functions mehHH
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  putStrLn "Checking monoids"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
