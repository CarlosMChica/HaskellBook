module MakeItBottom where

-- Make it bottom up with bang patterns or seq

x = undefined
y = "blah"

main = do
  print (snd (x, y))

main' = do
  print (snd (x, x `seq` y))
