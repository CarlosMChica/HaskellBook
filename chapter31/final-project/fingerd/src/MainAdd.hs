{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQLite
import qualified Database.SQLite.Simple.Types
import           System.Environment

data User =
  User {
         userId        :: Integer
       , username      :: Text
       , shell         :: Text
       , homeDirectory :: Text
       , realName      :: Text
       , phone         :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

insertUserQuery :: Query
insertUserQuery = "REPLACE INTO users VALUES (?, ?, ?, ?, ?, ?)"

getUserQuery :: Query
getUserQuery = "SELECT * from users WHERE userName = ?"

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Database.SQLite.Simple.Types.Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> return $ Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

insertUser :: Connection -> UserRow -> IO ()
insertUser dbConn user@(_, name, _, _, _, _) = do
  conn <- open "finger.db"
  execute conn insertUserQuery user
  getUser dbConn name >>= print
  SQLite.close conn

main :: IO ()
main = do
  (username: shell: homeDirectory: realName: phone : _) <- getArgs
  conn <- open "finger.db"
  insertUser conn (Database.SQLite.Simple.Types.Null,
                   T.pack username,
                   T.pack shell,
                   T.pack homeDirectory,
                   T.pack realName,
                   T.pack phone)
  SQLite.close conn
