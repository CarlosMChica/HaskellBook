{-# LANGUAGE TupleSections #-}
module Morra where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           System.Random

data GameMode = GameMode
  { p1Character :: Character,
    p2Character :: Character
  }
data Character = Human | Computer deriving Show
data Side = Odds | Evens deriving (Read, Eq)
data Player = P1 { playerData :: PlayerData} |
              P2 { playerData :: PlayerData}

data PlayerData = PlayerData {
    character :: Character,
    side      :: Side
  }
data Hand = Hand {
    player  :: Player,
    fingers :: Int
  }
data Turn = Turn {
    p1Hand :: Hand,
    p2Hand :: Hand
  }



winner :: Turn -> Player
winner = liftA3 playerInSide
                (player . p1Hand)
                (player . p2Hand)
                (winner . totalFingers)
  where totalFingers :: Turn -> Int
        totalFingers = liftA2 (+) (fingers . p1Hand) (fingers . p2Hand)
        winner :: Int -> Side
        winner count = if even count then Evens else Odds

playerInSide :: Player -> Player -> Side -> Player
playerInSide p1@(P1 p1Data) p2 side' = if side p1Data == side' then p1 else p2

playHand :: Player -> IO Hand
playHand = abc
  where hint = putStr "Show fingers: "
        abc player = case character . playerData $ player of
          Human    -> hint *> (Hand player <$> readLn)
          Computer -> Hand player <$> randomRIO (1, 5)

playTurn :: GameMode -> Side -> IO Turn
playTurn mode = liftA2 playTurn'
                       (P1 . PlayerData (p1Character mode))
                       (P2 . PlayerData (p2Character mode) . oposite)
  where playTurn' p1 p2 = liftA2 Turn (playHand p1) (playHand p2)

chooseGameMode :: IO GameMode
chooseGameMode = do
  putStr "1.- Human-Human * 2.- Human-Computer"
  mode <- (readLn :: IO Int)
  case mode of
    1 -> return $ GameMode Human Human
    2 -> return $ GameMode Human Computer

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
incrementScore (P1 _) game = game { p1Score = (p1Score game) + 1}
incrementScore (P2 _) game = game { p2Score = (p2Score game) + 1}

showScore :: GameState -> IO ()
showScore game =
  printWith " " ["P1 score:", show . p1Score $ game,
                 "-",
                 "P2 score", show . p2Score $ game]

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

play :: GameMode -> Side -> StateT GameState IO ()
play mode p1Side = do
  turn <- liftIO $ playTurn mode p1Side
  liftIO $ showTurn turn
  modify . incrementScore . winner $ turn
  get >>= liftIO . showScore

continue :: IO Bool
continue = do
  putStrLn "Continue?"
  readLn

repeatGame  :: GameMode -> Side -> IO ()
repeatGame mode p1Side = go (GameState 0 0)
  where go gameState = do
           nextState <- liftIO $ execStateT (play mode p1Side) gameState
           cont <- liftIO continue
           if cont then (go nextState) else return ()

main' :: IO ()
main' = do
  mode <- chooseGameMode
  p1Side <- chooseSide
  repeatGame mode p1Side

main :: IO ()
main = do
  evalStateT (mapM_ play' [(1 :: Int)..]) $ GameState 0 0
  where play' = const $ liftA2 play (liftIO chooseGameMode) (liftIO chooseSide)
