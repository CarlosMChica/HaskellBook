{-# LANGUAGE Strict #-}
module StrictTest where

blah x = 1

main = print (blah undefined)

willForce x = 1

willNotForce ~x = 1
