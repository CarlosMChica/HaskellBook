{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

-- 1
a = (*9) 6
b = head [(0, "doge"), (1, "kitteh")]
c = head [(0 :: Integer, "doge"), (1, "kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = length [1, 2, 3, 4] > length "TACOCAT"

-- 2
x = 5
y = x + 5
w = y + 10

-- 3
x1 = 5
y1 = x1 + 5
z y = y1 * 10

-- 4
x2 = 5
y2 = x2 + 5
f1 = 4 / y2

-- 5
x3 = "Julie"
y3 = " <3 "
z3 = "Haskell"
f2 = x3 ++ y3 ++ z3
