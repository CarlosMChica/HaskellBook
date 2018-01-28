module ChapterExercises.PhoneNumber where

import           Control.Applicative
import           Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

skipSeparator :: Parser ()
skipSeparator = skipMany . char $ '-'

digits :: Int -> Parser Int
digits n = read <$> count n digit

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
      try (char '(' *> digits 3  <* char ')')
  <|> try (digits 3 <* skipSeparator)
  <|> try (digits 3)
  <|> try (digit *> skipSeparator *> digits 3)

parseExchange :: Parser Exchange
parseExchange =
      try (skipSeparator *> digits 3 <* skipSeparator)
  <|> try (space         *> digits 3 <* skipSeparator)
  <|> try (                 digits 3                 )

parseLineNumber :: Parser LineNumber
parseLineNumber = digits 4

parsePhone :: Parser PhoneNumber
parsePhone = liftA3 PhoneNumber parseNumberingPlanArea parseExchange parseLineNumber

main :: IO ()
main = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"
