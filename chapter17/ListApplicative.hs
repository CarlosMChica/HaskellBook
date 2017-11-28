module ListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (Cons f fs) <*>  xs = append (fmap f xs) (fs <*>  xs)
  _ <*>  _                     = Nil

instance (Eq a) => EqProp (List a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    frequency $ [(1, return Nil),
                 (2, return $ Cons x xs)]

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
take' n (Cons x xs) = Cons x $ take' (n - 1) xs
take' 0 _           = Nil
take' _ Nil         = Nil

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ pure x
  (ZipList' (Cons f fs)) <*>  (ZipList' (Cons x xs)) = ZipList' $ Cons (f x) (fs <*> xs)
  _                      <*>   _                     = ZipList' Nil

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

trigger = undefined :: List (Int, Int, Int)
trigger' = undefined :: ZipList' (Int, Int, Int)

main :: IO()
main = do
  quickBatch (applicative trigger)
  quickBatch (applicative trigger')
