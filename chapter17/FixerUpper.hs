{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS -Wall #-}
module FixerUpper where

-- 1. const <$> Just "Hello" <*> "World"
f :: Maybe [Char]
f = const <$> Just "Hello" <*>  pure "World"

-- 2 (,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3].
g :: Maybe (Integer, Integer, [Char], [Integer])
g = (,,,) <$> Just 90 <*>  Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
