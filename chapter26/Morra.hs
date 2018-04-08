{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module Morra where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List
import           System.Random

data GameMode = HumanVsHuman | HumanVsComputer
data Character = Human | Computer deriving (Eq, Show)
data Side = Odds | Evens deriving (Read, Eq)
data Player = P1 { playerData :: PlayerData} |
              P2 { playerData :: PlayerData}

data PlayerData = PlayerData {
    character :: Character,
    moves     :: [Int],
    score     :: Int,
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

instance Show Player where
  show (P1 _) = "P1"
  show (P2 _) = "P2"

isHuman :: Player -> Bool
isHuman p = Human == (character . playerData $ p)

winner :: Turn -> Player
winner = liftA3 playerInSide
                (player . p1Hand)
                (player . p2Hand)
                (winnerSide . totalFingers)
  where totalFingers :: Turn -> Int
        totalFingers = liftA2 (+) (fingers . p1Hand) (fingers . p2Hand)
        winnerSide count = if even count then Evens else Odds

playerInSide :: Player -> Player -> Side -> Player
playerInSide p1@(P1 p1Data) p2 side' = if side p1Data == side' then p1 else p2
playerInSide p2@(P2 p2Data) p1 side' = if side p2Data == side' then p2 else p1

playHand :: Player -> GameState -> IO (Hand, [Int])
playHand player state = case character . playerData  $ player of
  Computer -> playComputerHand
  Human    -> playHumanHand
  where p1Moves = moves . playerData . p1 $ state
        playComputerHand = do
          hasPattern <- discoveredPattern p1Moves
          hand <- Hand player <$> (if hasPattern then playComputerHandSmart else randomRIO (1,5))
          return (hand, [])
        discoveredPattern moves | length moves < 5 = return False
        discoveredPattern moves = do
          let tail = [last . init $ moves, last moves]
              head = take 2 moves
          hasPattern <- return $ head == tail
          putStrLnWith " " ["Pattern discovered:", show hasPattern, "- HEAD", show head, "- TAIL", show tail]
          return hasPattern
        playComputerHandSmart = do
          smartPlay <- return $ last . take 3 $ p1Moves
          putStrLnWith " " ["Playing smart.", "Played ", show smartPlay]
          return smartPlay
        playHumanHand = do
          interstitial *> hint
          fingers <- readLn
          return $ (Hand player fingers, p1Moves ++ [fingers])
          where hint = putStrWith " " [show player, "show fingers: "]
                interstitial = if hasToShowInterstitial then showInterstitial else showNothing
                hasToShowInterstitial = case player of
                  (P2 _) | isHuman player -> True
                  _      -> False
                showInterstitial = replicateM_ 100 (putStrLn "")
                showNothing = return ()

playTurn :: GameState -> IO GameState
playTurn initialState = playTurn'
  where playTurn' = do
          (p1Hand, moves) <- playHand (p1 initialState) initialState
          (p2Hand, moves2) <- playHand (p2 initialState) initialState
          let turn = (Turn p1Hand p2Hand)
          return $ initialState {
            turns = turns initialState ++ [turn],
            p1 = p1 initialState,
            p2 = p2 initialState
            }

chooseGameMode :: IO GameMode
chooseGameMode = do
  putStr "1.- Human vs Human *** 2.- Human vs Computer: "
  mode <- (readLn :: IO Int)
  case mode of
    1 -> return $ HumanVsHuman
    2 -> return $ HumanVsComputer

chooseSide :: IO Side
chooseSide = hint *> readLn
  where hint = putStrWith " " ["P1", "choose side: Odds - Evens? "]

oposite :: Side -> Side
oposite Odds  = Evens
oposite Evens = Odds

putStrWith :: String -> [String] -> IO ()
putStrWith = printWith putStr

putStrLnWith :: String -> [String] -> IO ()
putStrLnWith = printWith putStrLn

printWith :: (String -> IO ()) -> String -> [String] -> IO ()
printWith f separator = f . concat . intersperse separator

data GameState = GameState
  {
    mode  :: GameMode,
    p1    :: Player,
    p2    :: Player,
    turns :: [Turn]
  }

addTurn :: Turn -> GameState -> [Turn]
addTurn turn gameState = turns gameState ++ [turn]

incrementPlayerScore :: Player -> Player
incrementPlayerScore player = player { playerData = incScore (playerData player) }
  where incScore pData = pData { score = (score pData) + 1}

incrementScore :: Player -> GameState -> GameState
incrementScore player1@(P1 _) game = game { p1 = incrementPlayerScore player1}
incrementScore player2@(P2 _) game = game { p2 = incrementPlayerScore player2}

showScore :: GameState -> IO ()
showScore state =
  putStrLnWith " " ["P1 score:", show . score . playerData . p1 $ state,
                 "-",
                 "P2 score:",show . score . playerData . p1 $ state]

showTurn :: Turn -> IO ()
showTurn turn = do
  putStrLnWith " " ["P1:", show p1Fingers,
                 "-",
                 "P2:", show p2Fingers,
                 "-",
                 "Total:", show totalFingers
                 ]
    where p1Fingers = fingers . p1Hand $ turn
          p2Fingers = fingers . p2Hand $ turn
          totalFingers = p1Fingers + p2Fingers

play :: StateT GameState IO ()
play = do
  initialState <- get
  newstate <- liftIO $ playTurn initialState
  liftIO $ putStrLnWith " " ["Accumlated moves:", show . moves . playerData . p1 $ newstate]
  let lastTurn = last . turns $ newstate
  liftIO $ showTurn lastTurn
  modify . incrementScore . winner $ lastTurn
  get >>= liftIO . showScore

continue :: IO Bool
continue = do
  putStrLn "Continue?"
  readLn

makeP1 :: Side -> Player
makeP1 = P1 . PlayerData Human [] 0

makeP2 :: GameMode -> Side -> Player
makeP2 gameMode = P2 . PlayerData charFromMode [] 0
  where charFromMode = case gameMode of
          HumanVsHuman    -> Human
          HumanVsComputer -> Computer

makeGameState :: IO GameState
makeGameState = do
  gameMode <- chooseGameMode
  p1Side <- chooseSide
  return $ GameState gameMode (makeP1 p1Side) (makeP2 gameMode (oposite p1Side)) []


repeatGame  :: IO ()
repeatGame = makeGameState >>= go
  where go game = do
           nextState <- liftIO $ execStateT play game
           cont <- continue
           when cont (go nextState)

main' :: IO ()
main' = repeatGame

main :: IO ()
main = makeGameState >>= evalStateT (mapM_ (const play) [(1 :: Int)..])
