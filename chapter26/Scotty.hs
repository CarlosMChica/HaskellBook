{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  hiding (get)
import           Data.Monoid                (mconcat)
import           Web.Scotty
import           Web.Scotty.Internal.Types  (ActionT (..))

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    lift . putStrLn $ "Hello"
    html $ mconcat ["<h1> Scotty, ", beam, " me up!</h1>"]

main2 = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    ActionT . lift . lift . lift . putStrLn $ "Hello"
    html $ mconcat ["<h1> Scotty, ", beam, " me up!</h1>"]

main3 = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    ActionT
      . (ExceptT . fmap Right)
      . lift
      . lift
      . putStrLn $ "Hello"
    html $ mconcat ["<h1> Scotty, ", beam, " me up!</h1>"]

main4 = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    ActionT
      . (ExceptT . fmap Right)
      . (ReaderT . const)
      . lift
      . putStrLn $ "Hello"
    html $ mconcat ["<h1> Scotty, ", beam, " me up!</h1>"]

main5 = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    ActionT
      . (ExceptT . fmap Right)
      . (ReaderT . const)
      . (\m -> StateT $ \s -> do
            a <- m
            return (a, s))
      . putStrLn $ "Hello"
    html $ mconcat ["<h1> Scotty, ", beam, " me up!</h1>"]
