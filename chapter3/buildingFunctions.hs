module BuildingFunctions where

exerciseTwoA :: String -> String
exerciseTwoA x = x ++ "!"

exerciseTwoB :: String -> String
exerciseTwoB x = take 4 (drop 1 x)

exerciseTwoC :: String -> String
exerciseTwoC =  drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2 

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String
rvrs           = three ++ " " ++ two ++ " " ++ one
  where string = "Curry is awesome"
        one    = take 5 string
        two    = take 2 $ drop 6 string
        three  = drop 9 string
