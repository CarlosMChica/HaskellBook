{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module ChapterExercises.Log where

import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict     as Map
import           Data.Monoid
import           Data.Time
import           Data.Time
import           Text.RawString.QQ
import           Text.Trifecta

type StartTime = UTCTime
type EndTime = UTCTime
type Duration = NominalDiffTime
type Activity = String

data ParsedEntry = ParsedEntry StartTime Activity deriving (Eq)
data ParsedLogDay = ParsedLogDay Day [ParsedEntry] deriving (Eq, Show)
type ParsedLog = [ParsedLogDay]

data TimePerActivity = TimePerActivity Activity NominalDiffTime deriving (Eq, Show)
data TimePerActivityPerDay = TimePerActivityPerDay Day NominalDiffTime deriving (Eq, Show)

data LogDayEntry = LogDayEntry {
  startTime :: StartTime,
  endTime   :: EndTime,
  duration  :: Duration,
  activity  :: Activity }
  deriving (Eq)

data LogDay = LogDay {
  day     ::Day,
  entries :: [LogDayEntry] }
  deriving (Eq, Show)

type Log = [LogDay]

instance Show LogDayEntry where
  show (LogDayEntry start end duration act) =
    intercalate " "
    [show start, show end, show duration, show act, "\n"]

instance Show ParsedEntry where
  show (ParsedEntry start act) = intercalate " " [show start, show act, "\n"]

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

digits :: Int -> Parser Int
digits n = read <$> count n digit

chars :: Int -> Parser String
chars n = count n digit

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipComment :: Parser ()
skipComment = do
  skipWhitespace
  string "--"
  skipMany (noneOf "\n")
  skipWhitespace

skipComments :: Parser ()
skipComments = skipWhitespace >> skipMany skipComment

parseDay :: Parser Day
parseDay = do
  year <- toInteger <$> digits 4
  skipSeparator
  month <- digits 2
  skipSeparator
  day <- digits 2
  skipComments
  return $ fromGregorian year month day
  where skipSeparator = skipMany $ char '-'

parseStartTime :: Day -> Parser UTCTime
parseStartTime day = do
  hour <- digits 2
  skipSeparator
  min <- digits 2
  skipWhitespace
  return $ toUTCTime day hour min
  where skipSeparator = skipMany $ char ':'
        toUTCTime day hour min = UTCTime day (secondsToDiffTime . inSecs hour $ min)
        inSecs hour min = toInteger (hour * 3600 + min * 60)

parseEntries :: Day -> Parser [ParsedEntry]
parseEntries day = some parseParsedEntry
  where parseParsedEntry = liftA2 ParsedEntry (parseStartTime day) parseActivity <* skipEOL
        parseActivity =
              try (manyTill (noneOf "\n") (skipComment))
          <|> (many $ noneOf "\n")

parseParsedLogDay :: Parser ParsedLogDay
parseParsedLogDay = do
  skipMarker
  day <- parseDay
  entries <- parseEntries day
  return $ ParsedLogDay day entries
  where skipMarker = char '#' >> space

parseLog :: Parser ParsedLog
parseLog = skipComments >> some parseParsedLogDay

unmarshallLog :: ParsedLog -> Log
unmarshallLog xs = xs >>= (\(ParsedLogDay day entries) -> return $ LogDay day $ logDays day entries)
  where logDays day entries = zipWith logDay' entries (tail entries ++  [lastParsedEntry day])
        lastParsedEntry day = ParsedEntry (UTCTime (addDays 1 day) 0) ""
        logDay' (ParsedEntry start act) (ParsedEntry start' _) = LogDayEntry start end (diffUTCTime end start) act
          where end = (addUTCTime (diffUTCTime start' start) start)


totalTimePerActivity :: Log  -> [TimePerActivity]
totalTimePerActivity log = timePerActivity <$> Map.toList (Map.fromListWith (+) timePerEntry)
  where timePerEntry = (\entry -> (activity entry, duration entry)) <$> allEntries log
        timePerActivity (activity, duration) = TimePerActivity activity duration
        allEntries log = foldMap entries log

averageTimePerActivityPerDay :: Log -> [TimePerActivityPerDay]
averageTimePerActivityPerDay log = dayAndAverage <$> log
  where dayAndAverage logDay = TimePerActivityPerDay (day logDay) (average . entries $ logDay)
        average entries = totalTimePerDay entries / (genericLength entries)
        totalTimePerDay entries = sum $ duration <$> entries

logSample :: String
logSample = [r|

-- wheee a comment


# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep



|]

main :: IO ()
main = do
  let (Success parsedLog) = parseString parseLog mempty logSample
      log  = unmarshallLog parsedLog
  putStrLn "***********************"
  putStrLn "Parsed Log"
  putStrLn "***********************"
  mapM_ print parsedLog
  putStrLn "***********************"
  putStrLn "Processed Log"
  putStrLn "***********************"
  mapM_ print log
  putStrLn "***********************"
  putStrLn "Total time per activity"
  putStrLn "***********************"
  mapM_ print . totalTimePerActivity $ log
  putStrLn "***********************"
  putStrLn "Average time per activity per day"
  putStrLn "***********************"
  mapM_ print . averageTimePerActivityPerDay $ log
