module Lookups where

-- 1.
-- added :: Maybe Integer
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2. Hmmm this type-check without changing anything
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
