module DerivingInstances where

data Trivial =
  Trivial

instance Eq Trivial where
  Trivial == Trivial = True

  -- not actually needed. Just to show how to write the definition using prefix notation for a infix operator
  (/=) Trivial Trivial = False

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Weds == Weds = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _   == _   = False
data Date =
  Date DayOfWeek Int deriving Show

instance Eq Date where
  (==) (Date dayOfWeek dayOfMonth) (Date dayOfWeek' dayOfMonth') =
    dayOfWeek == dayOfWeek' && dayOfMonth == dayOfMonth'

data Identity a = 
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity a) (Identity a') =
    a == a'
