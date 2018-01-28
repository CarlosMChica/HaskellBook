module ChapterExercises.Integer where

import           Control.Applicative
import           Text.Trifecta
-- 2. Positive Integer Parser

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"
--parseDigit =     char '0'
--             <|> char '1'
--             <|> char '2'
--             <|> char '3'
--             <|> char '4'
--             <|> char '5'
--             <|> char '6'
--             <|> char '7'
--             <|> char '8'
--             <|> char '9'

base10PosInteger :: Parser Integer
base10PosInteger = read <$> some parseDigit

-- 3. Extend to handle negative and positive integers

base10AllInteger :: Parser Integer
base10AllInteger = do
      try (char '-' >> base10PosInteger) >>= return . negate
  <|> base10PosInteger
--  sign <- char '-' <|> return '+'
--  int <- base10PosInteger
--  case sign of
--    '-' -> return $ negate int
--    '+' -> return int

main :: IO ()
main = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10PosInteger mempty "123abc"
  print $ parseString base10PosInteger mempty "abc"
  print $ parseString base10AllInteger mempty "-123abc"
