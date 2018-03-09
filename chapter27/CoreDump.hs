module Coredump where

data Test =
  A Test2 |
  B Test2
  deriving (Show)

data Test2 =
  C Int |
  D Int
  deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C x)) = x
forceTest2 (A (D x)) = x
forceTest2 (B (C x)) = x
forceTest2 (B (D x)) = x

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True  -> 1

-- Run with
-- :set -ddump-simpl
-- and
-- :set -dsuppress-all
-- to enable GHC Core

--A good example of "Outside-In evaluation"
--forceTest2 =
--    \ ds_d2j0 ->
--          case ds_d2j0 of _ {
--            A ds1_d2je ->
--              case ds1_d2je of _ {
  --              C x_a1ay -> x_a1ay; --x_a1ay is not evaluated, just returned
  --              D x_a1az -> x_a1az --x_a1az is not evaluated, just returned
--              };
      --      B ds1_d2jf ->
      --        case ds1_d2jf of _ {
      --          C x_a1aA -> x_a1aA; --x_a1aA is not evaluated, just returned
      --          D x_a1aB -> x_a1aB --x_a1aB is not evaluated, just returned
      --        }
--    }

main :: IO ()
main = do
  print $ forceNothing undefined -- Prints 0
  print $ forceNothing $ A undefined -- Prints 0
  print $ forceTest undefined -- Blows up
  print $ forceTest $ A undefined -- Prints 0
  print $ forceTest2 undefined -- Blows up
  print $ forceTest2 $ A undefined -- Blows up
  print $ forceTest2 $ A $ C undefined -- Blows up
  print $ forceTest2 $ A $ C 0 -- Prints 0
  return ()
