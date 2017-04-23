module OrderInstances where

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _   Fri = LT
  compare _   _   = EQ
