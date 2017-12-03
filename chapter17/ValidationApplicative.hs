{-# LANGUAGE DeriveAnyClass #-}
module ValidationApplicative where

import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Validation e a =
    Success' a
  | Failure' e
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success' x) = Success' $ f x
  fmap _ (Failure' x) = Failure' x

instance Semigroup e => Applicative (Validation e) where
  pure x = Success' x
  (Success' f) <*> (Success' x)   = Success' $ f x
  (Failure' x) <*> (Success' _)   = Failure' $ x
  (Success' _) <*> (Failure' x)   = Failure' $ x
  (Failure' x) <*> (Failure' x')  = Failure' $ x <> x'

trigger = undefined :: Validation (String, String, String) (String, String, String)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure' <$> arbitrary, Success' <$> arbitrary]

instance (Eq a, Eq e) => EqProp (Validation e a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch (applicative trigger)
