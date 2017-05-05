module Phone  where

import Data.Maybe
import Data.Char
import Data.Monoid
import qualified Data.Map as Map
import Data.List
import Data.Tuple

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

layout :: [(Char, (Digit, Presses))]
layout =
  [
    ('a', ('2', 1)),
    ('b', ('2', 2)),
    ('c', ('2', 3)),
    ('d', ('3', 1)),
    ('e', ('3', 2)),
    ('f', ('3', 3)),
    ('g', ('4', 1)),
    ('h', ('4', 2)),
    ('i', ('4', 3)),
    ('j', ('5', 1)),
    ('k', ('5', 2)),
    ('l', ('5', 3)),
    ('m', ('6', 1)),
    ('n', ('6', 2)),
    ('o', ('6', 3)),
    ('p', ('7', 1)),
    ('q', ('7', 2)),
    ('r', ('7', 3)),
    ('s', ('7', 4)),
    ('t', ('8', 1)),
    ('u', ('8', 2)),
    ('v', ('8', 3)),
    ('w', ('9', 1)),
    ('x', ('9', 2)),
    ('y', ('9', 3)),
    ('z', ('9', 4)),
    ('1', ('1', 1)),
    ('2', ('2', 4)),
    ('3', ('3', 4)),
    ('4', ('4', 4)),
    ('5', ('5', 4)),
    ('6', ('6', 4)),
    ('7', ('7', 5)),
    ('8', ('8', 4)),
    ('9', ('9', 5)),
    ('0', ('0', 1)),
    ('+', ('0', 2)),
    (' ', ('0', 3)),
    ('#', ('#', 1)),
    ('.', ('#', 2)),
    (',', ('#', 3))
  ]

phone = makePhone layout

capitalization :: (Digit, Presses)
capitalization = ('*', 1)

data Keyboard = Keyboard (Map.Map Char [(Digit, Presses)]) deriving (Show)
data DaPhone = DaPhone Keyboard deriving (Show)

type Digit = Char
type Presses = Int

makePhone :: [(Char, (Digit, Presses))] -> DaPhone
makePhone xs = DaPhone (Keyboard (Map.fromList $ capitalize =<< xs))
  where
    capitalize :: (Char, (Digit, Presses)) -> [(Char, [(Digit, Presses)])]
    capitalize (y, z) = 
      if isAlpha y 
      then [(y, [z]), (toUpper y, [capitalization, z])]
      else [(y, [z])]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone (Keyboard xs)) x = fromMaybe [] . Map.lookup x $ xs

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = foldMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . fmap snd

mostPopularLetterWithCount :: String -> (Char, Int)
mostPopularLetterWithCount xs =
  swap . maximum . fmap swap . Map.toList . Map.fromListWith (+) $ [(x, 1) | x <- xs, not . isSpace $ x]

mostPopularWordWithCount :: String -> (String, Int)
mostPopularWordWithCount xs =
  swap . maximum . fmap swap . Map.toList . Map.fromListWith (+) $ [(x, 1) | x <- words xs]

mostPopularLetter :: String -> Char
mostPopularLetter = fst. mostPopularLetterWithCount

mostPopularWord :: String -> String
mostPopularWord = fst . mostPopularWordWithCount

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . unwords

main = do
  mapM_ print . fmap (cellPhonesDead phone) $ convo
  print "Most popular letter in each sentence"
  mapM_ print . fmap mostPopularLetter $ convo
  print "Coolest letter in the text"
  print . coolestLtr $ convo
  print "Most popular word in each sentence"
  mapM_ print . fmap mostPopularWord $ convo
  print "Coolest word in the text"
  print . coolestWord $ convo
