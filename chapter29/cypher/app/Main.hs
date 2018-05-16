module Main where

import System.IO
import System.Exit
import Control.Monad
import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [mode, key] ->
      case mode of
        "-d" -> hPutStrLn stdout =<< (flip unVigenere key) <$> hGetLine stdin
        "-e" -> hPutStrLn stdout =<< (flip vigenere key) <$> hGetLine stdin
        otherwise -> hPutStrLn stderr "Unkown option" >> exitFailure
