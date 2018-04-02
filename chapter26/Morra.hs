{-# LANGUAGE TupleSections #-}
module Morra where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           Data.Semigroup
import           System.Random

data Side = Odds | Evens deriving (Read, Eq)
data Player = P1 { side :: Side} | P2 { side :: Side}
data Hand = Hand {
    player  :: Player,
    fingers :: Int
  }
data Turn = Turn {
    p1Hand :: Hand,
    p2Hand :: Hand
  }


winnerSide :: Turn -> Side
winnerSide = side' . getSum <$> (handFingers p1Hand) `mappend` (handFingers p2Hand)
  where handFingers hand = Sum <$> fingers . hand
        side' x = if even x then Evens else Odds

whoPlays :: Side -> Turn -> Player
whoPlays side' turn = if p1Side turn == side' then p1 else p2
  where p1 = player . p1Hand $ turn
        p2 = player . p2Hand $ turn
        p1Side = side . player . p1Hand

whoWin :: Turn -> Player
whoWin turn = whoPlays (winnerSide turn) turn

playHand :: Player -> IO Hand
playHand p@(P2    _) = Hand p <$> randomRIO (1, 5)
playHand p@(P1 _) = hint *> (Hand p <$> readLn)
  where hint = putStr "Show fingers: "

playTurn :: Side -> IO Turn
playTurn = (liftA2 playTurn' (P1) (P2 . oposite))
  where playTurn' p1 p2 = liftA2 Turn (playHand p1) (playHand p2)

congratulate :: Player -> IO ()
congratulate p = printWith " " ["Congrats", showPlayer p, "you win"]

showPlayer :: Player -> String
showPlayer (P1 _) = "P1"
showPlayer (P2 _) = "P2"

showScore :: Turn -> IO ()
showScore turn = do
  showHandScore p1Hand
  showHandScore p2Hand
  where showHandFingers = show . fingers
        showHandScore hand = printWith " " [showPlayer . player . hand $ turn, showHandFingers . hand $ turn, "fingers"]

chooseSide :: IO Side
chooseSide = hint *> readLn
  where hint = putStr "Choose side: Odds - Evens? "

oposite :: Side -> Side
oposite Odds  = Evens
oposite Evens = Odds

printWith :: String -> [String] -> IO ()
printWith separator = putStrLn . concat . intersperse separator

data GameState = GameState
  {
    p1Score :: Int,
    p2Score :: Int
  }

incrementScore :: Player -> GameState -> GameState
incrementScore (P1 _) game    = game { p1Score = (p1Score game) + 1}
incrementScore (P2    _) game = game { p2Score = (p2Score game) + 1}

showTurn :: Turn -> IO ()
showTurn turn = do
  printWith " " ["P1 fingers:", show p1Fingers,
                 "-",
                 "P2 fingers:", show p2Fingers,
                 "-",
                 "Total fingers:", show totalFingers
                 ]
    where p1Fingers = fingers . p1Hand $ turn
          p2Fingers = fingers . p2Hand $ turn
          totalFingers = p1Fingers + p2Fingers

play  :: StateT GameState IO ()
play = do
  turn <- liftIO $ chooseSide >>= playTurn
  liftIO $ showTurn turn
  modify . incrementScore . whoWin $ turn
  get >>= liftIO . showScore'

showScore' :: GameState -> IO ()
showScore' game = do
  printWith " " ["P1 score:", show . p1Score $ game,
                 "-",
                 "P2 score", show . p2Score $ game]

continue :: IO Bool
continue = do
  putStrLn "Continue?"
  readLn

main' :: IO ()
main' = repeatGame

repeatGame  :: IO ()
repeatGame = go (GameState 0 0)
  where go game = do
           nextState <- liftIO $ execStateT play game
           cont <- liftIO continue
           if cont then (go nextState) else return ()

main :: IO ()
main = do
  evalStateT (mapM_ (const play) [(1 :: Int)..]) $ GameState 0 0
