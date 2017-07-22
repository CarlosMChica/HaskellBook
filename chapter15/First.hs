module First where

import Optional
import Data.Monoid
import MonoidProps
import Test.QuickCheck

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada

  mappend (First' Nada)     (First' Nada)     = First' Nada
  mappend (First' (Only a)) _                 = First' $ Only a
  mappend (First' _)        (First' (Only a)) = First' $ Only a

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [
                (9, return $ First' (Only x)),
                (1, return $ First' Nada)
              ]
type A = String
type FirstMappend = First' A -> First' A -> First' A -> Bool

main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' A -> Bool)
  quickCheck (monoidRightIdentity :: First' A -> Bool)
