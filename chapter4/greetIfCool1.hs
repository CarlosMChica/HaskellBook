module GreetIfCool1 where

greeIfCool :: String -> IO ()
greeIfCool coolness = 
  if cool coolness
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool x = x == "downright frosty yo"
