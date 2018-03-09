{-# LANGUAGE TupleSections #-}
module Morra where

import           Control.Monad.Trans.State
import           Data.Int
import           Data.Semigroup
import           System.Random

data Player = Human | AI
data Hand = Hand {
    player  :: Player,
    fingers :: Int
  }
data Turn = Turn {
    human :: Hand,
    ai    :: Hand
  }

winsTurn :: Turn -> Player
winsTurn turn =
  if (even $ (fingers . human $ turn) + (fingers . ai $ turn))
  then player $  ai turn
  else player $ human turn

play :: Player -> IO Hand
play p@Human = Hand p <$> readLn
play p@AI    = Hand p <$> randomRIO (1, 5)

congratulate :: Player -> IO ()
congratulate Human = putStrLn "Congrats Human you win"
congratulate AI    = putStrLn "Congrats AI you win"

showScore :: Turn -> IO ()
showScore (Turn human ai) = do
  putStrLn . concat $ ["Human: " , show $ fingers human, " fingers"]
  putStrLn . concat $ ["AI: " , show $ fingers ai, " fingers"]

main :: IO ()
main = do
  human <- play Human
  ai    <- play AI
  turn  <- return $ Turn human ai
  showScore turn
  congratulate $ winsTurn turn
