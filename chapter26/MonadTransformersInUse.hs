{-# LANGUAGE OverloadedStrings #-}

module MonadTransformersInUse where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text, pack)
import qualified Data.Text.Lazy             as TL
import           Web.Scotty

param' :: Parsable a => Text -> ActionM (Maybe a)
param' text = rescue (Just <$> param text)
                     (const . return $ Nothing)

main = scotty 3000 $ do
  get "/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

param'' :: Parsable a => Text -> MaybeT ActionM a
param'' = MaybeT . param'

type Reco = (Integer, Integer, Integer, Integer)

main' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      r <- param'' "1"
      e <- param'' "2"
      c <- param'' "3"
      o <- param'' "4"
      lift . lift . print $ e
      return ((r, e, c ,o) :: Reco)
    liftIO . print $ reco
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

param''' :: Parsable a => Text -> ActionM (Either Text a)
param''' text = rescue (Right <$> param text)
                       (\error -> return . Left $ error)

main'' = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    a <- param''' "1"
    let a' = either (const (0 :: Integer)) id a
    liftIO $ print (a)
    liftIO $ print (a')
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

param'''' :: Parsable a => Text -> ExceptT Text ActionM a
param'''' = ExceptT . param'''

main''' = scotty 3000 $ do
  get "/" $ do
    reco <- runExceptT $ do
      r <- param'''' "1"
      e <- param'''' "2"
      c <- param'''' "3"
      o <- param'''' "4"
      lift . lift . print $ e
      return ((r, e, c ,o) :: Reco)
    liftIO . print $ reco
    case reco of
      Left e  -> text e
      Right r -> html $ mconcat ["<h1>Success! Reco was:" , pack . show $ r, "</h1>"]
