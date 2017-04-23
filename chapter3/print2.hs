module Print2 where

main :: IO ()
main = do
  putStrLn "count to four for me:"
  putStr   "one, two"
  putStr   ", three and"
  putStrLn "four!"
