module MonoidProps where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = a <> (b <> c) == (a <> b) <> c

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = m == m <> mempty

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mempty <> m == m

type S = String
type B = Bool

sampleCheckMonoidsProps = do
  quickCheck (monoidAssoc :: S -> S -> S -> B)
  quickCheck (monoidLeftIdentity :: S -> B)
  quickCheck (monoidRightIdentity :: S -> B)
