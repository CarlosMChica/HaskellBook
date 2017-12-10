module ChapterExercises where

import           Control.Applicative
import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

--1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _   = NopeDotJpg
  _ <*> _  = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  _ >>= _  = NopeDotJpg

--2.

data PbtEither b a =
    Left1 a
  | Right1 b deriving (Eq, Show)

instance Functor (PbtEither b) where
  fmap f (Left1 x)  = Left1 $ f x
  fmap f (Right1 y) = Right1 y

instance Applicative (PbtEither b) where
  pure x = Left1 x
  (Left1 f)  <*> (Left1 x)   = Left1 $ f x
  (Right1 y) <*> (Right1 y') = Right1 y'
  _          <*> (Right1 y)  = Right1 y
  (Right1 y) <*> _           = Right1 y

instance Monad (PbtEither b) where
  return = pure
  (Left1 x)  >>= f = f x
  (Right1 y) >>= _ = (Right1 y)

--3.

data Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> x = fmap f x

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

--4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil         = Nil

instance Applicative List where
  pure x = Cons x Nil
  (Cons f fs) <*> xs = (f <$> xs) `append1` (fs <*> xs)
  _           <*> _ = Nil

instance Monad List where
  return x = Cons x Nil
  xs >>= f = join1 (f <$> xs)

concat1 :: List (List a) -> List a
concat1 Nil           = Nil
concat1 (Cons xs xxs) = xs `append1` (concat1 xxs)

append1 :: List a -> List a -> List a
append1 xs           Nil = xs
append1 Nil          xs  = xs
append1 (Cons x xs)  xs' = Cons x (xs `append1` xs')

--1

join1 :: Monad m => m (m a) -> m a
join1 = (=<<) id

--2

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

--3

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

--4

a :: Monad m => m a -> m (a -> b) -> m [b]
a x f = liftA pure (f <*> x)


--5

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh x f = sequence' . fmap f $ x

sequence' :: (Applicative m) => [m a] -> m [a]
sequence' xs = foldr (liftA2 (:)) (pure []) xs

--6

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance (Arbitrary a, Arbitrary b) => Arbitrary (PbtEither b a) where
  arbitrary = oneof [Left1 <$> arbitrary, Right1 <$> arbitrary]

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil , liftA2 Cons arbitrary arbitrary]

instance EqProp (Nope a) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (PbtEither b a) where (=-=) = eq
instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Eq a => EqProp (List a) where (=-=) = eq

nopeTrigger = undefined :: Nope (Int, Int, Int)
pbtEitherTrigger = undefined :: PbtEither (Int, Int, Int) (Int, Int, Int)
identityTrigger = undefined :: Identity (Int, Int, Int)
listTrigger = undefined :: List (Int, Int, Int)

main :: IO ()
main = do
  quickBatch (applicative listTrigger)
  quickBatch (monad listTrigger)
  quickBatch (applicative identityTrigger)
  quickBatch (monad identityTrigger)
  quickBatch (applicative pbtEitherTrigger)
  quickBatch (monad pbtEitherTrigger)
  quickBatch (applicative nopeTrigger)
  quickBatch (monad nopeTrigger)
