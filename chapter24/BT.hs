{-# LANGUAGE OverloadedStrings #-}

module BT where

import           Control.Applicative
import           Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import           Data.ByteString            (ByteString)
import           Text.Parsec                (Parsec, parseTest)
import           Text.Trifecta              hiding (parseTest)

trifP :: Show a => Parser a -> String -> IO ()
trifP parser input = print $ parseString parser mempty input

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP parser input = print $ parseOnly parser input

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main = do
  putStrLn "Trifecta"
  trifP nobackParse "13"
  trifP tryParse "13"
  trifP tryAnnot "13"

  putStrLn "Parsec"
  parsecP nobackParse "13"
  parsecP tryParse "13"
  parsecP tryAnnot "1"

  putStrLn "Attoparsec"
  attoP nobackParse "13"
  attoP tryParse "13"
  attoP tryAnnot "3"
