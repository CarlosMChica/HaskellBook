module FunctorLaws where

import Test.QuickCheck.Function

functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fun _ f) (Fun _ g) x = fmap (g . f) x == (fmap g . fmap f $ x)
