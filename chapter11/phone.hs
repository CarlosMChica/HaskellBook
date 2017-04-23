module Phone  where

import Data.Maybe
import Data.Char
import Data.Monoid

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

data Keyboard = Keyboard [(Char, [(Digit, Presses)])] deriving (Show)
data DaPhone = DaPhone Keyboard deriving (Show)

type Digit = Char
type Presses = Int

makePhone :: [(Char, (Digit, Presses))] -> DaPhone
makePhone xs = DaPhone (Keyboard (capitalize =<< xs))
  where
    capitalize :: (Char, (Digit, Presses)) -> [(Char, [(Digit, Presses)])]
    capitalize (y, z) = case isAlpha y of
      True  -> [(y, [z]), (toUpper y, [capitalization, z])]
      False -> [(y, [z])]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone (Keyboard xs)) x = fromMaybe [] . lookup x $ xs

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead phone = foldMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . fmap snd

main = mapM_ print . fmap (cellPhonesDead phone) $ convo
