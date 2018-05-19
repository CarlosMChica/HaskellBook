module StopTheParty where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad      (forever)
import           System.Random      (randomRIO)

randomException :: IO ()
randomException = do
  i <- randomRIO (1, 10 :: Int)
  if i `elem` [1..9]
     then throwIO DivideByZero
     else throwIO StackOverflow

main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  _ <- tryS randomException
  putStrLn "Live to loop another day!"
  -- microseconds
  threadDelay (1 * 1000000)

-- Can make main loop infinitely by make tryS Either SomeException ()
