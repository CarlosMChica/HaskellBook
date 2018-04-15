#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.5
  --package random
  --package transformers
-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module Morra where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.List
import           Data.Monoid
import           System.Random

data GameMode = HumanVsHuman | HumanVsComputer deriving Show
data Character = Human | Computer deriving (Eq, Show)
data Side = Odds | Evens deriving (Read, Eq, Show)
data Player = P1 { playerData :: PlayerData} |
              P2 { playerData :: PlayerData} deriving Show

data PlayerData = PlayerData {
    character :: Character,
    moves     :: [Int],
    score     :: Int,
    side      :: Side
  } deriving Show

data Turn = Turn {
    p1Fingers :: Int,
    p2Fingers :: Int
  } deriving Show

data GameState = GameState
  {
    mode  :: GameMode,
    p1    :: Player,
    p2    :: Player,
    turns :: [Turn]
  } deriving Show

isHuman :: Player -> Bool
isHuman p = Human == (character . playerData $ p)

winner :: GameState -> Player
winner = liftA3 playerInSide
                p1
                p2
                (winnerSide . totalFingers . last . turns)
  where totalFingers :: Turn -> Int
        totalFingers = liftA2 (+) p1Fingers p2Fingers
        winnerSide count = if even count then Evens else Odds

playComputerHand :: GameState -> IO Int
playComputerHand gameState = if canPlaySmart humanMoves then playComputerHandSmart humanMoves else randomRIO (1,5)
  where humanMoves = moves . playerData . p1 $ gameState
        canPlaySmart humanMoves | length humanMoves < 4 = False
        canPlaySmart humanMoves = first2HumanMoves == last2HumanMoves
          where last2HumanMoves = [last . init $ humanMoves, last humanMoves]
                first2HumanMoves = take 2 humanMoves
        playComputerHandSmart p1Moves = do
          smartPlay <- return $ last . take 3 $ p1Moves
          putStrLnWith " " ["Playing smart.", "Played ", show smartPlay]
          return smartPlay

playHumanHand :: Player -> IO Int
playHumanHand player = do
  interstitial *> hint
  readLn
  where hint = putStrWith " " [formatPlayer player, "show fingers: "]
        interstitial = if hasToShowInterstitial then replicateM_ 100 (putStrLn "") else return ()
        hasToShowInterstitial = case player of
          (P2 _) | isHuman player -> True
          _      -> False
        formatPlayer (P1 _) = "P1"
        formatPlayer (P2 _) = "P2"

playTurn :: GameState -> IO Turn
playTurn gameState = liftA2 Turn player1Hand player2Hand
  where player1Hand = playHand $ p1 gameState
        player2Hand = playHand $ p2 gameState
        playHand player = case character . playerData  $ player of
          Computer -> playComputerHand gameState
          Human    -> playHumanHand player

chooseGameMode :: IO GameMode
chooseGameMode = do
  putStr "1.- Human vs Human *** 2.- Human vs Computer: "
  option <- (readLn :: IO Int)
  case option of
    1 -> return $ HumanVsHuman
    2 -> return $ HumanVsComputer

chooseSide :: IO Side
chooseSide = hint *> readLn
  where hint = putStrWith " " ["P1", "choose side: Odds - Evens? "]

putStrWith :: String -> [String] -> IO ()
putStrWith = printWith putStr

putStrLnWith :: String -> [String] -> IO ()
putStrLnWith = printWith putStrLn

printWith :: (String -> IO ()) -> String -> [String] -> IO ()
printWith f separator = f . concat . intersperse separator

playerInSide :: Player -> Player -> Side -> Player
playerInSide p1@(P1 p1Data) p2 side' = if side p1Data == side' then p1 else p2
playerInSide p2@(P2 p2Data) p1 side' = if side p2Data == side' then p2 else p1

incrementScore :: Player -> GameState -> GameState
incrementScore (P1 _) (GameState m pl1 pl2 ts) = GameState m (incrementP1Score pl1) pl2 ts
  where incrementP1Score (P1 (PlayerData char moves score side)) = P1 $ PlayerData char moves (score + 1) side
incrementScore (P2 _) (GameState m pl1 pl2 ts) = GameState m pl1 (incrementP2Score pl2) ts
  where incrementP2Score (P2 (PlayerData char moves score side)) = P2 $ PlayerData char moves (score + 1) side

addTurn :: Turn -> GameState -> GameState
addTurn t (GameState m pl1 pl2 ts) = GameState m pl1 pl2 (ts ++ [t])

updatePlayers :: Turn -> GameState -> GameState
updatePlayers t (GameState m pl1 pl2 ts) = GameState m (addMove pl1 $ p1Fingers t) (addMove pl2 $ p2Fingers t) ts
  where addMove (P1 (PlayerData char moves score side)) move = P1 $ PlayerData char (moves ++ [move]) score side
        addMove (P2 (PlayerData char moves score side)) move = P2 $ PlayerData char (moves ++ [move]) score side


play :: StateT GameState IO ()
play = do
  newTurn <- get >>= liftIO . playTurn
  liftIO . showTurn $ newTurn
  modify $ appEndo $
    (Endo $ (flip incrementScore) <*> winner) <>
    (Endo $ updatePlayers newTurn) <>
    (Endo $ addTurn newTurn)
  get >>= liftIO . showScore
  where showScore gameState = putStrLnWith " " ["P1 score:", show . score . playerData . p1 $ gameState,
                                                "-",
                                                "P2 score:",show . score . playerData . p2 $ gameState]
        showTurn turn = putStrLnWith " " ["P1:", show $ p1Fingers turn,
                                           "-",
                                           "P2:", show $ p2Fingers turn,
                                           "-",
                                           "Total:", show $ p1Fingers turn + p2Fingers turn
                                         ]
continue :: IO Bool
continue = do
  putStrLn "Continue?"
  readLn

makeP1 :: Side -> Player
makeP1 s = P1 $ PlayerData Human [] 0 s

makeP2 :: GameMode -> Side -> Player
makeP2 gameMode s = P2 $ PlayerData charFromMode [] 0 s
  where charFromMode = case gameMode of
          HumanVsHuman    -> Human
          HumanVsComputer -> Computer

makeGameState :: IO GameState
makeGameState = do
  gameMode <- chooseGameMode
  p1Side <- chooseSide
  return $ GameState gameMode (makeP1 p1Side) (makeP2 gameMode (oposite p1Side)) []
  where oposite Odds  = Evens
        oposite Evens = Odds

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
