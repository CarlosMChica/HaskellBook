module HeavyLifting where

import Control.Applicative

-- 1
-- a = (+1) $ read "[1]" :: [Int]

a = (+1) <$> read "[1]" :: [Int]

-- 2
-- b = (++ "lol") (Just ["Hi,", "Hello"])

b = fmap (++ "lol") <$> Just ["Hi,", "Hello"]

-- 3
-- c = (*2) (\x -> x - 2)

c :: Num a => a -> a
c = (*2) . (\x -> x - 2)

-- 4
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])

d  = ((['1'] ++) . show) <$> (\x -> [x, 1..3])
d' = (return '1' ++) . show . (\x -> [x, 1..3])

-- 5
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++) show ioi
--     in (*3) changed

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (read . ("123" ++)) <$> (show <$> ioi)
    in (*3) <$> changed

main = do
  print a
  print b
  print . c $ 1
  print . d $ 0
  e 
