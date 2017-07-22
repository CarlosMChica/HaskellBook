module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  mempty = Listy []
  mappend (Listy xs) (Listy ys) = Listy $ xs `mappend` ys

