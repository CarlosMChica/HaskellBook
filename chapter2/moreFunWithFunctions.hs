module MoreFunWithFunctions where

z = 7 

x = y ^ 2

-- waxOn = x * 5

y = z + 8

waxOn = x * 5 
  where z = 7
        x = y ^ 2
        y = z + 8
        
triple x = x * 3

wallOff = triple
