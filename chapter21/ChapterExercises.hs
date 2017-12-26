module ChapterExercises where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes



newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x



newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = liftA Constant (pure x)



data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f (Yep x) = Yep $ f x
  fmap _ Nada    = Nada

instance Foldable Optional where
  foldMap f (Yep x) = f x
  foldMap _ Nada    = mempty

instance Traversable Optional where
  traverse f (Yep x) = liftA Yep (f x)
  traverse _ Nada    = pure Nada



data List a = Cons a (List a) | Nil deriving (Eq, Show)

instance Functor List where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ Nil         = Nil

instance Foldable List where
  foldMap f (Cons x xs) = f x <> (foldMap f xs)
  foldMap _ Nil         = mempty

instance Traversable List where
  traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)
  traverse _ Nil         = pure Nil



data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = (Three x y (f z))

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = liftA (Three x y) (f z)



data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y y') = Three' x (f y) (f y')

instance Foldable (Three' a) where
  foldMap f (Three' x y y') = f y <> f y'

instance Traversable (Three' a) where
  traverse f (Three' x y y') = liftA2 (Three' x) (f y) (f y')



data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S t x) = S (f <$> t) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S t x) = (foldMap f t) <> (f x)

instance Traversable n => Traversable (S n) where
  traverse f (S n x) = liftA2 S (traverse f n) (f x)

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Node left x right) = Node (f <$> left) (f x) (f <$> right)
  fmap f (Leaf x)            = Leaf $ f x
  fmap _ Empty               = Empty

instance Foldable Tree where
 foldMap f (Node left x right) = (foldMap f left) <> f x <> (foldMap f right)
 foldMap f (Leaf x)            = f x
 foldMap _ Empty               = mempty

instance Traversable Tree where
  traverse f (Node left x right) = liftA3 Node (traverse f left) (f x) (traverse f right)
  traverse f (Leaf x)            = liftA Leaf (f x)
  traverse _ Empty               = pure Empty

-- QuickCheck

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada), (3, Yep <$> arbitrary)]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, liftA2 Cons arbitrary arbitrary)]

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = liftA2 S arbitrary arbitrary

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
    [ (1, return Empty),
      (2, liftA Leaf arbitrary),
      (3, liftA3 Node arbitrary arbitrary arbitrary)
    ]


instance Eq a => EqProp (Identity a) where (=-=) = eq
instance Eq a => EqProp (Constant a b) where (=-=) = eq
instance Eq a => EqProp (Optional a) where (=-=) = eq
instance Eq a => EqProp (List a) where (=-=) = eq
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq
instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq
instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq
instance Eq a => EqProp (Tree a) where (=-=) = eq


triggerIdentity = undefined :: Identity (Int, Int, [Int])
triggerConstant = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
triggerOptional = undefined :: Optional (Int, Int, [Int])
triggerList     = undefined :: List (Int, Int, [Int])
triggerThree    = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
triggerThree'    = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
triggerS        = undefined :: S [] (Int, Int, [Int])
triggerTree = undefined :: Tree (Int, Int, [Int])

main :: IO ()
main = do
  quickBatch (traversable triggerIdentity)
  quickBatch (traversable triggerConstant)
  quickBatch (traversable triggerOptional)
  quickBatch (traversable triggerList)
  quickBatch (traversable triggerThree)
  quickBatch (traversable triggerThree')
  quickBatch (traversable triggerS)
  quickBatch (traversable triggerTree)
