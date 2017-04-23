module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft' 

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft'
 
eftInt :: Int -> Int -> [Int]
eftInt = eft'

eftChar :: Char -> Char -> String
eftChar = eft'

eft' :: (Ord a, Enum a) => a -> a -> [a]
eft' x y
        | x > y = []
        | otherwise = go x y []
              where go x y acc
                      | x == y    = acc ++ [x]
                      | otherwise = go (succ x) y (acc ++ [x])
