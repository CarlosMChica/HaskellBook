module RollYourOwn where

import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go sum count gen
         | sum >= n  = count
         | otherwise = let (nextV, nextG) = randomR (1, 6) gen
                       in go (sum + nextV) (count + 1) nextG

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = (fmap . fmap) intToDie (go 0 0 [] g)
  where go sum count dies gen
          | sum >= n   = (count, dies)
          | otherwise = let (die, nextG) = randomR (1, 6) gen
                        in go (sum + die) (count + 1) (die : dies) nextG
