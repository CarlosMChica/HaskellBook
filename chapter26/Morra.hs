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
        playerInSide pl1 pl2 side' = if (side . playerData $ pl1) == side' then pl1 else pl2

winnerSide :: Integral a => a -> Side
winnerSide count = if even count then Evens else Odds

boolf :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
boolf h f g x = if h x then f x else g x

andf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andf f g x = f x && g x

eqf :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqf f g x = f x == g x

takeSafe :: Int -> [a] -> Maybe [a]
takeSafe n xs | n <= length xs = Just $ take n xs
takeSafe _ _  = Nothing

takeLastSafe :: Int -> [a] -> Maybe [a]
takeLastSafe n xs | n == length xs = Just xs
takeLastSafe n xs | length xs > n = go n xs
  where go c ys | c > 0 = (++ [last ys]) <$> go (c - 1) (init ys)
        go _ _  = Just []
takeLastSafe _ _                  = Nothing

elemAt :: Int -> [a] -> Maybe a
elemAt n xs | length xs > 0 && length xs > n = Just $ xs !! n
elemAt _ _  = Nothing

safeInit :: [a] -> Maybe [a]
safeInit xs | length xs > 1 = Just $ init xs
safeInit _  = Nothing

orElse :: Maybe a -> a -> a
orElse mx x = maybe x id mx

playComputerHand :: GameState -> IO Int
playComputerHand gameState = playSmart `orElse` playRandom
  where playSmart = if isComputerSide Odds then smartGuess Odds else smartGuess Evens
        smartGuess computerSide = do
          firstMoves   <- takeSafe     3 humanMoves
          lastMoves    <- takeLastSafe 3 humanMoves
          humanMove    <- elemAt       3 humanMoves
          initialGuess <- elemAt       3 humanMoves
          let canSmartGuess = firstMoves == lastMoves && length humanMoves >= 6
          case canSmartGuess of
              False -> Nothing
              True  -> Just $ do
                let guess = if (computerSide == winnerSide (humanMove + initialGuess))
                            then initialGuess
                            else (succ initialGuess)
                putStrLnWith "" ["Playing smart - Played: ", show guess]
                return guess
        isComputerSide side' = side' == (side . playerData . p2 $ gameState)
        playRandom = randomRIO (1,5)
        humanMoves = moves . playerData . p1 $ gameState

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
          Computer -> playComputerHand gameState
          Human    -> playHumanHand player
          where playerChar = character . playerData  $ player

chooseGameMode :: IO GameMode
chooseGameMode = do
  putStr "1.- Human vs Human *** 2.- Human vs Computer: "
  option <- (readLn :: IO Int)
  case option of
    1 -> return $ HumanVsHuman
    2 -> return $ HumanVsComputer
    _ -> chooseGameMode

chooseSide :: IO Side
chooseSide = hint *> readLn
  where hint = putStrWith " " ["P1", "choose side: Odds - Evens? "]

incrementScore :: GameState -> Player -> GameState
incrementScore gameState (P1 _) = gameState { p1 = incrementPScore (p1 gameState) }
incrementScore gameState (P2 _) = gameState { p2 = incrementPScore (p2 gameState) }

incrementPScore :: Player -> Player
incrementPScore player = player
  {
    playerData = (playerData player)
    {
      score = (score (playerData player)) + 1
    }
  }

addTurn :: Turn -> GameState -> GameState
addTurn t (GameState m pl1 pl2 ts) = GameState m pl1 pl2 (ts ++ [t])

updatePlayers :: Turn -> GameState -> GameState
updatePlayers turn gameState = gameState
  {
    p1 = addMove (p1 gameState) (p1Fingers turn),
    p2 = addMove (p2 gameState) (p2Fingers turn)
  }
  where addMove player move = player
          {
            playerData = (playerData player)
            {
              moves = (moves (playerData player)) ++ [move]
            }
          }

play :: StateT GameState IO ()
play = do
  turn <- get >>= liftIO . playTurn
  liftIO $ showTurn turn
  modify $ (incrementScore <*> winner) . (updatePlayers turn) . (addTurn turn)
  get >>= liftIO . showScore
  where showScore gameState =
          let p1Score = show . score . playerData . p1
              p2Score = show . score . playerData . p2
          in  putStrLnWith " " ["P1 score:", p1Score gameState, "-", "P2 score:", p2Score gameState]
        showTurn turn =
          let totalFingers = show $ p1Fingers turn + p2Fingers turn
              fingersP1 = show . p1Fingers
              fingersP2 = show . p2Fingers
          in  putStrLnWith " " ["P1:", fingersP1 turn , "-", "P2:", fingersP2 turn, "-", "Total:", totalFingers]

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

putStrWith :: String -> [String] -> IO ()
putStrWith = printWith putStr

putStrLnWith :: String -> [String] -> IO ()
putStrLnWith = printWith putStrLn

printWith :: (String -> IO ()) -> String -> [String] -> IO ()
printWith f separator = f . concat . intersperse separator

main' :: IO ()
main' = repeatGame

main :: IO ()
main = makeGameState >>= evalStateT (mapM_ (const play) [(1 :: Int)..])
