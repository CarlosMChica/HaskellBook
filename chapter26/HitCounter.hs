{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           System.Environment         (getArgs)
import           Web.Scotty.Trans


data Config =
  Config {
     counts :: IORef (M.Map Text Integer)
   , prefix :: Text
   }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
           -> M.Map Text Integer
           -> (M.Map Text Integer, Integer)
bumpBoomp text map = (M.insert text bumped map, bumped)
  where bumped = 1 + (fromMaybe 0 $ M.lookup text map)
--bumpBoomp text map = (M.insertWith (+) text 1 map, fromMaybe 1 $ M.lookup text map)

--app :: Scotty ()
--app =
--  get "/:key" $ do
--    unprefixed <- param "key"
--    config <- lift . ReaderT $ \config -> do
--          let key' = mappend (prefix config) unprefixed
--          modifyIORef (counts config) (fst . bumpBoomp key')
--          return config
--    let key' = mappend (prefix config) unprefixed
--    newMap <- liftIO . readIORef . counts $ config
--    newInteger <- liftIO . return $ fromMaybe 0 $ M.lookup key' newMap
--    html $ mconcat [ "<h1>Success! Count was: ",
--                     TL.pack $ show newInteger,
--                     "</h1>"
--                   ]
--
--main :: IO ()
--main = do
--  [prefixArg] <- getArgs
--  counter <- newIORef M.empty
--  let config = Config counter $ TL.pack prefixArg
--      runR reader = runReaderT reader config
--  scottyT 300 runR app

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    config <- lift ask
    let key' = mappend (prefix config) unprefixed
        mapRef = counts config
        map = readIORef mapRef
    (newMap, newInteger) <- liftIO $ bumpBoomp key' <$> map
    liftIO $ writeIORef mapRef newMap
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
