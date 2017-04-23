module ThyFearfulSymmetry where

import Data.Char (isSpace)

myWords :: String -> [String]
myWords x = go x []
      where go [] words = words
            go x words = go rest (words ++ [word])
              where word = takeWhile (/=' ') x
                    rest = dropWhile (==' ') $ dropWhile (/=' ') x

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = go x []
      where go [] words = words
            go x words = go rest (words ++ [word])
              where word = takeWhile (/='\n') x
                    rest = dropWhile (=='\n') $ dropWhile (/='\n') x

shouldEqual = [ "Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"
              ]

main :: IO ()
main =
  print $ "Are they Equal? "
          ++ show (myLines sentences == shouldEqual)
