module Listy where

data Listy a = Listy [a]
               deriving (Eq, Show)

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy xs) (Listy ys) = Listy $ xs `mappend` ys
