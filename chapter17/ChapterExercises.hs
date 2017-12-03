module ChapterExercises where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

--1.
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance Applicative Pair where
  pure x = Pair x x
  (Pair f f') <*> (Pair x x') = Pair (f x) (f' x')

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x $ f y

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two x f) <*> (Two x' y) = Two (x <> x') (f y)

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y $ f z

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x                            = Three mempty mempty x
  (Three x y f) <*> (Three x' y' z) = Three (x <> x') (y <> y') (f z)

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' x f f') <*> (Three' x' y y') = Three' (x <> x') (f y) (f' y')

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z t) = Four x y z $ f t

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four x y z f) <*> (Four x' y' z' t) = Four (x <> x') (y <> y') (z <> z') (f t)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x x' x'' y) = Four' x x' x'' $ f y

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' x y z f) <*> (Four' x' y' z' t) = Four' (x <> x') (y <> y') (z <> z') (f t)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftA2 Two arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

pairTrigger = undefined :: Pair (String, String, String)
twoTrigger = undefined :: Two (String, String, String) (String, String, String)
threeTrigger = undefined :: Three (String, String, String) (String, String, String) (String, String, String)
three'Trigger = undefined :: Three' (String, String, String) (String, String, String)
fourTrigger = undefined :: Four (String, String, String) (String, String, String) (String, String, String) (String, String, String)
four'Trigger = undefined :: Four' (String, String, String) (String, String, String)

main :: IO ()
main = do
  quickBatch (applicative pairTrigger)
  quickBatch (applicative twoTrigger)
  quickBatch (applicative threeTrigger)
  quickBatch (applicative three'Trigger)
  quickBatch (applicative fourTrigger)
  quickBatch (applicative four'Trigger)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos x y z = liftA3 (,,) x y z
