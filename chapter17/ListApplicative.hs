module ListApplicative where

import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (f <$> xs)

instance Monoid (List a) where
  mempty = Nil
  xs `mappend` ys = xs `append` ys

instance Applicative List where
  pure x            = Cons x Nil
  _         <*> Nil = Nil
  Nil       <*> _   = Nil
  Cons f fs <*> xs   = (f <$> xs) <> (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil         = acc
fold f acc (Cons x xs) = f x (fold f acc xs)

concat' :: (List (List a)) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 _           = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

zipWith' :: (a -> b -> c) -> (List a) -> (List b) -> (List c)
zipWith' _ Nil _                   = Nil
zipWith' _ _ Nil                   = Nil
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)

instance Applicative ZipList' where
  pure x                           = ZipList' $ repeat' x
  _ <*> (ZipList' Nil)             = ZipList' Nil
  (ZipList' Nil) <*> _             = ZipList' Nil
  (ZipList' xs)  <*> (ZipList' ys) = ZipList' (zipWith' id xs ys)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = frequency $
    [
      (1, return Nil),
      (2, Cons <$> arbitrary <*> arbitrary)
    ]

--instance Arbitrary a => Arbitrary (List a) where
--  arbitrary = Cons <$> arbitrary <*> arbitrary
trigger = undefined :: List (Int, Int, Int)
trigger' = undefined :: ZipList' (Int, Int, Int)

main :: IO()
main = do
  quickBatch (applicative trigger)
  quickBatch (applicative trigger')
