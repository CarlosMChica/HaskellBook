{-# LANGUAGE NoMonomorphismRestriction #-}

module DoesItCompile where

-- 1
bigNum = (^) 5 $ 10
wahoo = bigNum + 10

-- 2
x = print
y = print "woohoo!"
z = x "Hello world"

-- 3
a = (+)
b = 5
c = a b 10
d = a c 200

-- 4
a1 = 12 + b1
b1 = 10000 * c1
c1 = 1
