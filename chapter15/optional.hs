module Optional where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty =  Nada
  x        `mappend` Nada     = x
  Nada     `mappend` y        = y
  (Only x) `mappend` (Only y) = Only (x `mappend` y)

