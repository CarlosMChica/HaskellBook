module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

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

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show puzzle@(Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++
    "\nCorrectly guessed so far: " ++ correctlyGuessed puzzle
    ++
    "\nWrongly guessed so far: " ++ wronglyGuessed puzzle

wronglyGuessed :: Puzzle -> String
wronglyGuessed (Puzzle _ discovered guessed) =
  guessed >>= \x -> [x | Just x `notElem` discovered]

correctlyGuessed :: Puzzle -> String
correctlyGuessed (Puzzle _ discovered guessed) =
  guessed >>= \x -> [x | Just x `elem` discovered]

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar _        = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle xs = Puzzle word initDiscovered []
  where word           = fmap toLower xs
        initDiscovered = fmap (const Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discoveredSoFar guessedSoFar) guess =
  Puzzle word newDiscoveredSoFar (guess : guessedSoFar)
    where newDiscoveredSoFar =
            zipWith (\x y -> if guess == x then Just x else y) word discoveredSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (alreadyGuessed puzzle guess,
        charInWord puzzle guess) of
    (True, _   )  -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (_   , True)  -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return $ fillInCharacter puzzle guess
    (_   , False) -> do
      putStrLn "This character was not in the word, try again."
      return $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle word _ guessed) =
  when (length (wronglyGuessed puzzle) > 7) $ do 
       putStrLn "You lose!"
       putStrLn $ "The word was: " ++ word
       exitSuccess

wronglyGuessedLimitReached :: Puzzle -> Bool
wronglyGuessedLimitReached puzzle =
  length (wronglyGuessed puzzle) >= wrongGuessesLimit

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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word  <- randomWord'
  runGame $ freshPuzzle word
