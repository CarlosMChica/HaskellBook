module ChapterExercises where

import           Data.Monoid

--1

data Constant' a b = Constant' a

instance Foldable (Constant' a) where
  foldMap _ _ = mempty

--2

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

--3

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ y z) = f z

--4

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ y y') = f y <> f y'

--5

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ y y' y'') = f y <> f y' <> f y''

filterF :: (Applicative f, Foldable t, Monoid (f a))
         => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

main :: IO ()
main = do
  --                                             t a           f a
  firstGreaterThanOne  <- return (filterF (>1) [1, 2, 3] :: Data.Monoid.First Int)
  sumGreaterThanOne    <- return (filterF (>1) [1, 2, 3] :: Data.Monoid.Sum Int)
  greaterThanOneToList <- return (filterF (>1) (Just 1)  :: [Int])
  print firstGreaterThanOne
  print sumGreaterThanOne
  print greaterThanOneToList
