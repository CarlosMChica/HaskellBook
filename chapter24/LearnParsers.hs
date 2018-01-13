module LearnParsers where

import           Control.Applicative
import           Text.Parser.Combinators
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1' <* eof

-- read a single character '1', then die
one' = one >> stop

-- read two characters, '1', and '2'
oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

--2
oneTwoThree :: Parser Char
oneTwoThree = choice [char '1', char '1' >> char '2', char '1' >> char '2' >>  char '3']

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneTwoThree:"
  testParse oneTwoThree
