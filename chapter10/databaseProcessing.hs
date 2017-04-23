module DatabaseProcessing where

import Data.Time
import Data.List(foldl')

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbNumber 1,
    DbNumber 2,
    DbNumber 10,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = fmap map . filter onlyDate
  where map (DbDate x)      = x
        onlyDate (DbDate _) = True
        onlyDate _          = False

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr accumulateTime []
  where accumulateTime (DbDate x) xs = x : xs
        accumulateTime _          xs = xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr accumulateNumber []
  where accumulateNumber (DbNumber x) xs = x : xs
        accumulateNumber _            xs = xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f base
  where base = UTCTime (fromGregorian 1000 1 1) (secondsToDiffTime 123)
        f (DbDate a) b = max a b
        f _          b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = foldl' f 0
  where f acc (DbNumber x) = acc + x
        f acc _            = acc

avgDb :: [DatabaseItem] -> Double
avgDb [] = 0
avgDb xs = (fromIntegral . sumDb $ xs) / (fromIntegral . length . filterDbNumber $ xs)



