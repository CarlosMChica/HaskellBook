module LetsWriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = fst $ divMod x 10
          d     = snd $ divMod xLast 10

hunsD :: Integral a => a -> a
hunsD x = d
    where x1 = fst $ divMod x 100
          d  = snd $ divMod x1 10

foldBool :: a -> a -> Bool -> a
foldBool a a' b =
  case b of
    True -> a
    False -> a'

foldBool' :: a -> a -> Bool -> a
foldBool' a a' b
  | b = a
  | otherwise = a'

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

main = do
  print . roundTrip $ 4
  print . id $ 4
