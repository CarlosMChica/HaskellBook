module Try where

import           Control.Applicative
import           Data.Ratio          ((%))
import           Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  return (numerator % denominator)

type RationalOrDecimal = Either Rational Integer

parseRationalOrDecimal :: Parser RationalOrDecimal
parseRationalOrDecimal = try (Left <$> parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  print $ parseString parseRationalOrDecimal mempty "1"
  print $ parseString parseRationalOrDecimal mempty "1/2"
  print $ parseString parseRationalOrDecimal mempty "1/0"
