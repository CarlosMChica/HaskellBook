module EitherMonad where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second $ f y

instance Applicative (Sum a) where
  pure x = Second x
  (First x)  <*> _          = First x
  _          <*> (First x)  = First x
  (Second f) <*> (Second y) = Second $ f y

instance Monad (Sum a) where
  return = pure
  (First x)  >>= _ = First x
  (Second y) >>= f = f y

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

trigger = undefined :: Sum (Int, Int, Int) (Int, Int, Int)

main :: IO ()
main = do
  quickBatch (applicative trigger)
  quickBatch (monad trigger)
