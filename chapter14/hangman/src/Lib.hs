{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList $ lines dict

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

wrongGuessesLimit :: Int
wrongGuessesLimit = 7

gameWords :: IO WordList
gameWords = do
  WordList words <- allWords
  return $ WordList $ filter gameLength words
  where gameLength w =
          let l = length w
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  index <- randomRIO (0, length wl - 1)
  return $ wl !! index

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

type DiscoveredSoFar = [Maybe Char]
type WordToDiscover = String
type GuessedSoFar = String
type Guess = Char
type WrongGuesses = [Guess]
type GoodGuesses = [Guess]
data Puzzle = Puzzle WordToDiscover DiscoveredSoFar GuessedSoFar
              deriving (Eq)

instance Show Puzzle where
  show puzzle@(Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++
    "\nCorrectly guessed so far: " ++ correctlyGuessed puzzle
    ++
    "\nWrongly guessed so far: " ++ wronglyGuessed puzzle
    where renderPuzzleChar :: Maybe Char -> Char
          renderPuzzleChar (Just x) = x
          renderPuzzleChar _        = '_'

wronglyGuessed :: Puzzle -> WrongGuesses
wronglyGuessed (Puzzle _ discovered guessed) =
  guessed >>= \x -> [x | Just x `notElem` discovered]

correctlyGuessed :: Puzzle -> GoodGuesses
correctlyGuessed (Puzzle _ discovered guessed) =
  guessed >>= \x -> [x | Just x `elem` discovered]

freshPuzzle :: WordToDiscover -> Puzzle
freshPuzzle xs = Puzzle word initDiscovered []
  where word           = fmap toLower xs
        initDiscovered = fmap (const Nothing) word


alreadyGuessed :: Puzzle -> Guess -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

fillInGuess :: Puzzle -> Guess -> Puzzle
fillInGuess (Puzzle word discoveredSoFar guessedSoFar) guess =
  Puzzle word newDiscoveredSoFar (guess : guessedSoFar)
    where newDiscoveredSoFar =
            zipWith (\x y -> if guess == x then Just x else y) word discoveredSoFar

-- From here on should probably be in the App.Main module as it's really the client of the Lib module, not part of it?

handleGuess :: Puzzle -> Guess -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (alreadyGuessed puzzle guess,
        guessInWord puzzle guess) of
    (True, _   )  -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (_   , True)  -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return $ fillInGuess puzzle guess
    (_   , False) -> do
      putStrLn "This character was not in the word, try again."
      return $ fillInGuess puzzle guess
  where guessInWord :: Puzzle -> Char -> Bool
        guessInWord (Puzzle word _ _) char = char `elem` word

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) =
  when (length (wronglyGuessed puzzle) > 7) $ do
       putStrLn "You lose!"
       putStrLn $ "The word was: " ++ word
       exitSuccess

wronglyGuessedLimitReached :: Puzzle -> Bool
wronglyGuessedLimitReached puzzle =
  (length . wronglyGuessed $ puzzle) >= wrongGuessesLimit

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  if all isJust discovered then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"
