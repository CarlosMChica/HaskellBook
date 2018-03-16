{-# LANGUAGE TupleSections #-}
module Morra where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Int
import           Data.List
import           Data.Semigroup
import           System.Random

data Side = Odds | Evens deriving (Read, Eq)
data Player = Human { side :: Side} | AI { side :: Side}
data Hand = Hand {
    player  :: Player,
    fingers :: Int
  }
data Turn = Turn {
    human :: Hand,
    ai    :: Hand
  }


sideFromInt :: Int -> Side
sideFromInt x = if even x then Evens else Odds

winnerSide :: (Turn -> Hand) -> (Turn -> Hand) -> Turn -> Side
winnerSide p1 p2 = side' . getSum <$> (handFingers p1) `mappend` (handFingers p2)
  where handFingers hand = Sum <$> fingers . hand
        side' x = if even x then Evens else Odds

whoPlays :: Side -> Turn -> Player
whoPlays side' turn = if humanSide turn == side' then human' else ai'
  where human' = player . human $ turn
        ai'    = player . ai $ turn
        humanSide = side . player . human

whoWin :: Turn -> Player
whoWin inTurn = whoPlays winnerSide' inTurn
  where winnerSide' = winnerSide human ai inTurn

playHand :: Player -> IO Hand
playHand p@(AI    _) = Hand p <$> randomRIO (1, 5)
playHand p@(Human _) = hint *> (Hand p <$> readLn)
  where hint = putStrLn "Show fingers:"

playTurn :: Side -> IO Turn
playTurn side = (liftA2 playTurn' (Human) (AI . oposite)) $ side
  where playTurn' human ai = liftA2 Turn (playHand human) (playHand ai)

congratulate :: Player -> IO ()
congratulate player = printWith " " ["Congrats", showPlayer player, "you win"]

showPlayer :: Player -> String
showPlayer (Human _) = "Human"
showPlayer (AI _)    = "AI"

showScore :: Turn -> IO ()
showScore turn = do
  showHandScore human
  showHandScore ai
  where showHandFingers = show . fingers
        showHandScore hand = printWith " " [showPlayer . player . hand $ turn, showHandFingers . hand $ turn, "fingers"]

chooseSide :: IO Side
chooseSide = hint *> readLn
  where hint = putStrLn "Choose side: Odds - Evens?"

oposite :: Side -> Side
oposite Odds  = Evens
oposite Evens = Odds

printWith :: String -> [String] -> IO ()
printWith separator = putStrLn . concat . intersperse separator

play' :: IO ()
play' = chooseSide >>= playTurn >>= whoWin' >>= congratulate
  where whoWin' turn = (showScore turn) *> (return . whoWin $ turn)

play :: IO ()
play = chooseSide' >=> playTurn >=> whoWin' >=> congratulate $ trigger
  where chooseSide' = const chooseSide
        whoWin'     = showScore *> return . whoWin
        trigger     = undefined

main :: IO ()
main = play'
