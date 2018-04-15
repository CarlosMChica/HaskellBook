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
import           Data.Bool
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
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
  where totalFingers = liftA2 (+) p1Fingers p2Fingers
        winnerSide count = if even count then Evens else Odds
        playerInSide pl1 pl2 side' = if (side . playerData $ pl1) == side' then pl1 else pl2

boolf :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
boolf h f g x = if h x then f x else g x

andf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andf f g x = f x && g x

eqf :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqf f g x = f x == g x

takeSafe :: Int -> [a] -> Maybe [a]
takeSafe n xs | n == length xs = Just xs
takeSafe n xs | length xs > n = go n xs
  where go c ys | c > 0 = ([head ys] ++) <$> go (c - 1) (tail ys)
        go _ ys = Just ys
takeSafe _ _                  = Nothing

takeLastSafe :: Int -> [a] -> Maybe [a]
takeLastSafe n xs | n == length xs = Just xs
takeLastSafe n xs | length xs > n = go n xs
  where go c ys | c > 0 = (++ [last ys]) <$> go (c - 1) (init ys)
        go _ ys = Just ys
takeLastSafe _ _                  = Nothing

elemAt :: Int -> [a] -> Maybe a
elemAt n xs | length xs > 0 && length xs > n = Just $ xs !! n
elemAt _ _  = Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs | length xs > 1 = Just $ init xs
safeInit _  = Nothing

orElse :: Maybe a -> a -> a
orElse mx x = maybe x id mx

playComputerHand :: [Int] -> IO Int
playComputerHand = boolf canPlaySmart playSmart playRandom
  where canPlaySmart = liftA2 (\x y -> traceShow x x && traceShow y y)
                              (\xs -> traceShow (xs) ((> 4) . length $ xs))
                              (liftA2 (\xs ys -> xs == ys && isJust xs) first2HumanMoves last2HumanMoves)
          where last2HumanMoves xs = traceShow (takeLastSafe 2 xs) (takeLastSafe 2 xs)
                first2HumanMoves xs = traceShow (takeSafe 2 xs) (takeSafe 2 xs)
        playSmart hMoves = do
          smartPlay <- return $ elemAt 3 hMoves `orElse` 0
          putStrLnWith " " ["Playing smart.", "Played ", show smartPlay]
          return smartPlay
        playRandom = const $ randomRIO (1,5)

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
        playHand player = case playerChar of
          Computer -> playComputerHand humanMoves
          Human    -> playHumanHand player
          where humanMoves = moves . playerData . p1 $ gameState
                playerChar = character . playerData  $ player

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

incrementScore :: Player -> GameState -> GameState
incrementScore (P1 _) (GameState m pl1 pl2 ts) = GameState m (incrementPScore pl1) pl2 ts
incrementScore (P2 _) (GameState m pl1 pl2 ts) = GameState m pl1 (incrementPScore pl2) ts


incrementPScore :: Player -> Player
incrementPScore player = player {
                           playerData = (playerData player) {
                                score = (score (playerData player)) + 1
                                }
                           }

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
