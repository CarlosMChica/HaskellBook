{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

import GHC.Arr
import FunctorLaws
import Test.QuickCheck
import Test.QuickCheck.Function

type IntToString = Fun Int String
type StringToInt = Fun String Int

-- 1.

-- data Bool = False | True
-- No valid Functor instance possible as Bool is not a higher kinded type, it's kind is *, functor needs * -> *

-- 2.

data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' $ f x
  fmap f (True' x)  = True' $ f x

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    x <- arbitrary
    oneof [return $ False' x, return $ True' x]

type BoolAndSomethingElseId = BoolAndSomethingElse String -> Bool
type BoolAndSomethingElseCompose = StringToInt -> IntToString -> BoolAndSomethingElse String -> Bool

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- This is equivalent to Maybe and I'm lazy so I'm not writing it again :). It's a perfectly valid functor.

-- 4.

newtype Mu f = Inf { outF :: f (Mu f) }

-- Not Possible as Mu has kind (* -> *) -> * and Functor requires kind * -> *.
-- (* -> *) -> * This kind means that the type argument f is itself a higher-kinded type. Applying f would leave kind *.

-- 5.

data D = D (Array Word Word) Int Int

-- No valid Functor instance possible as Bool is not a higher kinded type, it's kind is *, functor needs * -> *


-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1.

-- Given type
-- data Sum a b = First a | Second b

-- Fixed type
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.

-- Given type
-- data Company a b c = DeepBlue a c | Something b

-- Fixed type
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something $ f b
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.

-- Given type
-- data More a b = L a b a | R b a b deriving (Eq, Show)

-- Fixed type
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--
--
-- Write Functor instances for the following datatypes.

-- 1.

data Quant a b =
    Finance
  | Desk a
  | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor x) = Bloor $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [Finance, Desk x, Bloor y]

type QuantId = Quant Int String -> Bool
type QuantCompose = IntToString -> StringToInt -> Quant String Int -> Bool

-- 2.

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    x <- arbitrary
    return $ K x

type KId = K Int Int -> Bool
type KCompose = IntToString -> StringToInt -> K Int Int -> Bool

-- 3.

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' $ f x

instance (Arbitrary b) => Arbitrary (Flip K' a b) where
  arbitrary = do
    x <- arbitrary
    return $ Flip $ K' x

type FlipKId = Flip K' Int Int -> Bool
type FlipKCompose = IntToString -> StringToInt -> Flip K' String Int -> Bool

-- 4.

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
 fmap f (GoatyConst x) = GoatyConst $ f x

-- This is clearly a valid Functor implementation so I won't bother writing proof for it.

-- 5.

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

-- Ideally this instance would work for any type, but I couldn't make it work
-- instance (Arbitrary a) => Arbitrary (LiftItOut f a) where
--   arbitrary = do
--     x <- arbitrary
--     return $ LiftItOut x

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    x <- arbitrary
    elements [LiftItOut (Just x), LiftItOut Nothing]

type LiftItOutId = LiftItOut Maybe Int -> Bool
type LiftItOutCompose = IntToString -> StringToInt -> LiftItOut Maybe Int -> Bool

-- 6.

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
 fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- Ideally this instance would work for any type, but I couldn't make it work
-- instance (Arbitrary a) => Arbitrary (Parappa f g a) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     return $ DaWrappa x y

instance (Arbitrary a) => Arbitrary (Parappa [] [] a) where
  arbitrary = do
    x  <- arbitrary
    x' <- arbitrary
    return $ DaWrappa [x] [x']

type ParappaId = Parappa [] [] Int -> Bool
type ParappaCompose = IntToString -> StringToInt -> Parappa [] [] Int -> Bool

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor g) => Functor (IgnoreOne f g a) where
 fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne [] [] a b) where
  arbitrary = do
    x  <- arbitrary
    y <- arbitrary
    return $ IgnoringSomething [x] [y]

type IgnoreOneId = IgnoreOne [] [] Int String -> Bool
type IgnoreOneCompose = StringToInt -> IntToString -> IgnoreOne [] [] Int String -> Bool

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

instance (Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious [] o a t) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Notorious [x] [y] [z]

type NotoriousId = Notorious [] Int String [Double] -> Bool

-- Deal with it :)                                              g  ----o--------  ---a---- ---t--
type NotoriousCompose = StringToInt -> IntToString -> Notorious [] (Maybe String) [Double] String -> Bool

-- 9.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x  <- arbitrary
    xs <- arbitrary
    frequency [(1, return Nil), (4, return $ Cons x xs)]

type ListId = List Int -> Bool
type ListCompose = StringToInt -> IntToString -> List String -> Bool

-- 10.

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat            = NoGoat
  fmap f (OneGoat x)       = OneGoat $ f x
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- Hangs. Don't really get why, my understanding is that this case is the same as exercise 9, although it seems it's not.
-- instance (Arbitrary a) => Arbitrary (GoatLord a) where
--   arbitrary = do
--     x   <- arbitrary
--     f   <- arbitrary
--     f'  <- arbitrary
--     f'' <- arbitrary
--     frequency [(1, return NoGoat), (2, return $ OneGoat x), (2, return $ MoreGoats f f' f'')]

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    frequency [
      (1, return NoGoat),
      (2, return $ OneGoat w),
      (2, return $ MoreGoats (OneGoat x) (OneGoat y) (OneGoat z))]


type GoatLordId = GoatLord Int -> Bool
type GoatLordCompose = StringToInt -> IntToString -> GoatLord String -> Bool

-- 11.

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)
  -- Can't derive Eq & Show instances. Don't know how to create one either.
  -- deriving (Eq, Show)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x (f y)
  fmap f (Read g) = Read (f . g)

instance (Arbitrary a) => Arbitrary (TalkToMe a) where
  arbitrary = do
    x <- arbitrary
    s <- arbitrary
    f <- arbitrary
    frequency [
      (1, return Halt),
      (2, return $ Print s x),
      (2, return $ Read f)]

main = do
  quickCheck (functorIdentity :: BoolAndSomethingElseId)
  quickCheck (functorCompose :: BoolAndSomethingElseCompose)
  quickCheck (functorIdentity :: QuantId)
  quickCheck (functorCompose :: QuantCompose)
  quickCheck (functorIdentity :: KId)
  quickCheck (functorCompose :: KCompose)
  quickCheck (functorIdentity :: FlipKId)
  quickCheck (functorCompose :: FlipKCompose)
  quickCheck (functorIdentity :: LiftItOutId)
  quickCheck (functorCompose :: LiftItOutCompose)
  quickCheck (functorIdentity :: ParappaId)
  quickCheck (functorCompose :: ParappaCompose)
  quickCheck (functorIdentity :: IgnoreOneId)
  quickCheck (functorCompose :: IgnoreOneCompose)
  quickCheck (functorIdentity :: NotoriousId)
  quickCheck (functorCompose :: NotoriousCompose)
  quickCheck (functorIdentity :: ListId)
  quickCheck (functorCompose :: ListCompose)
  quickCheck (functorIdentity :: GoatLordId)
  quickCheck (functorCompose :: GoatLordCompose)
