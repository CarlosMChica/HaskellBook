module OurExceptions where

import           Control.Exception

data NotDivThree = NotDivThree Int deriving (Eq, Show)

instance Exception NotDivThree

data NotEven = NotEven Int deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i = throwIO $ NotEven i
  | otherwise = return i

catchNotDivThree :: IO Int -> (NotDivThree -> IO Int) -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int -> (NotEven -> IO Int) -> IO Int
catchNotEven = catch

catchBoth :: IO Int -> IO Int
catchBoth action = catches action [ Handler $ \(NotDivThree _) -> return maxBound,
                                    Handler $ \(NotEven _) -> return minBound
                                  ]

data EATD = NotEven' Int | NotDivThree' Int deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv' :: Int -> IO Int
evenAndThreeDiv' i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i = throwIO $ NotEven i
  | otherwise = return i


catchBoth' :: IO Int -> (EATD -> IO Int) -> IO Int
catchBoth' = catch
