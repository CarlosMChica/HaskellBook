module Main where

import Test.Hspec
import Test.QuickCheck
import Lib

genNonEmptyAlphaString :: Gen WordToDiscover
genNonEmptyAlphaString = listOf1 genAlphaChar
  where genAlphaChar :: Gen Char
        genAlphaChar = elements ['a'..'z']

main :: IO ()
main = hspec $
  describe "Hangman" $ do
    describe "fillInGuess" $ do

      it "fills single good guess occurrence" $ do
        let word = "word"
        let guess     = 'w'
        let puzzle    = Puzzle word [Nothing,  Nothing, Nothing, Nothing] ""
        let newPuzzle = Puzzle word [Just 'w', Nothing, Nothing, Nothing] [guess]
        fillInGuess puzzle guess `shouldBe` newPuzzle

      it "fills multiple good guess occurrences" $ do
        let word = "worwdw"
        let guess     = 'w'
        let puzzle    = Puzzle word [Nothing,  Nothing, Nothing, Nothing, Nothing, Nothing] ""
        let newPuzzle = Puzzle word [Just 'w', Nothing, Nothing, Just 'w', Nothing, Just 'w'] [guess]
        fillInGuess puzzle guess `shouldBe` newPuzzle

      it "does not fill bad guesses" $ do
        let word = "word"
        let guess     = 'x'
        let puzzle    = Puzzle word [Nothing, Nothing, Nothing, Nothing] ""
        let newPuzzle = Puzzle word [Nothing, Nothing, Nothing, Nothing] [guess]
        fillInGuess puzzle guess `shouldBe` newPuzzle

    -- test printing to console?
    describe "handleGuess" $ do

      it "handles single good guess occurrence" $
        let word = "word"
            guess          = 'w'
            puzzle         = Puzzle word [Nothing,  Nothing, Nothing, Nothing] ""
            expectedPuzzle = Puzzle word [Just 'w', Nothing, Nothing, Nothing] [guess]
          in do
            newPuzzle <- handleGuess puzzle guess
            newPuzzle `shouldBe` expectedPuzzle

      it "handles multiple good guess occurrences" $ do
        let word = "worwdw"
        let guess     = 'w'
        let puzzle    = Puzzle word [Nothing,  Nothing, Nothing, Nothing, Nothing, Nothing] ""
        let expectedPuzzle = Puzzle word [Just 'w', Nothing, Nothing, Just 'w', Nothing, Just 'w'] [guess]
         in do
            newPuzzle <- handleGuess puzzle guess 
            newPuzzle `shouldBe` expectedPuzzle

      it "handles bad guesses" $ do
        let word = "word"
        let guess     = 'x'
        let puzzle    = Puzzle word [Nothing, Nothing, Nothing, Nothing] ""
        let expectedPuzzle = Puzzle word [Nothing, Nothing, Nothing, Nothing] [guess]
          in do
            newPuzzle <- handleGuess puzzle guess 
            newPuzzle `shouldBe` expectedPuzzle

      it "handles duplicated guesses" $ do
        let word = "word"
        let guess     = 'x'
        let puzzle    = Puzzle word [Nothing, Nothing, Nothing, Nothing] ""
        let expectedPuzzle = Puzzle word [Nothing, Nothing, Nothing, Nothing] [guess]
          in do
            newPuzzle1 <- handleGuess puzzle guess
            newPuzzle2 <- handleGuess newPuzzle1 guess
            newPuzzle2 `shouldBe` newPuzzle1

-- Tried a property testing approach for the test above.
-- As an exercise, add required functions to Lib module in order to expose properties 

-- letterNotInWord :: String -> Char
-- letterNotInWord word = head [ x |
--                               x <- ['a'..'z'],
--                               x `notElem` word ]
-- 
-- prop_goodGuesses :: Property
-- prop_goodGuesses =
--   forAll genNonEmptyAlphaString
--   (\word@(guess : _) -> do
--     let puzzle    = Puzzle word [Nothing,  Nothing, Nothing, Nothing] ""
--     let newPuzzle = Puzzle word [Nothi]
--     fillInGuess puzzle guess == newPuzzle)
-- 
-- prop_badGuesses :: Property
-- prop_badGuesses =
--   forAll genNonEmptyAlphaString
--   (\word -> do
--     let puzzle = freshPuzzle word
--     let badGuess = letterNotInWord word
--     let newPuzzle = fillInGuess puzzle badGuess
--     wronglyGuessed newPuzzle == [badGuess])
-- 
-- quickCheck' :: Property -> String -> IO ()
-- quickCheck' prop msg = do
--   putStrLn msg
--   quickCheck prop
-- 
-- main :: IO ()
-- main = do
--   putStrLn ""
--   quickCheck' prop_goodGuesses "good guesses"
--   quickCheck' prop_badGuesses "bad guesses"
