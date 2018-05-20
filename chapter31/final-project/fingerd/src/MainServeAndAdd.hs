{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad                (forever)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQLite
import qualified Database.SQLite.Simple.Types
import           Network.Socket               hiding (close, recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           Text.RawString.QQ

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

createUsers :: Query
createUsers = [r|
  CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     userName TEXT UNIQUE,
     shell TEXT,
     homeDirectory TEXT,
     realName TEXT,
     phone TEXT)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users wHERE userName = ?"

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

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUserQuery meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow = (Database.SQLite.Simple.Types.Null,
                 "carlos",
                 "/bin/bash/",
                 "home/carlos",
                 "Carlos Morera",
                 "999-999-9999")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName phone) = BS.concat
  [ "Login: ", e username,
    "\t\t\t\t",
    "Name: ", e realName, "\n",
    "Directory: ", e homeDir,
    "\t\t\t",
    "Shell: ", e shell, "\n",
    "Phone : ", e phone, "\n"
  ]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn $ "Couldn't find matching user for username: " ++ (show username)
      return ()
    Just user -> do
      putStrLn $ "User: " ++ show user
      sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name   -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  sClose soc

handleAdd :: Connection -> Socket -> IO ()
handleAdd dbConn soc = do
  msg <- recv soc 1024
  print msg
  case T.splitOn " " $ T.strip . decodeUtf8 $ msg of
    [username, shell, home, realName, phone] ->
      insertUser dbConn (Database.SQLite.Simple.Types.Null, username, shell, home, realName, phone)
    _ -> sendAll soc $ "Incorrect format. Usage: 'username shell home realname phone'"


handleAdds :: Connection -> Socket -> IO ()
handleAdds dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling addition"
  handleAdd dbConn soc
  sClose soc

insertUser :: Connection -> UserRow -> IO ()
insertUser dbConn user@(_, name, _, _, _, _) = do
  execute dbConn insertUserQuery user
  getUser dbConn name >>= print
  SQLite.close dbConn

serve :: String -> (Connection -> Socket -> IO ()) -> IO ()
serve port handler = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing
    (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
                Stream
                defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  handler conn sock
  SQLite.close conn
  sClose sock

main :: IO ()
main = do
  _ <- forkIO $ serve "79" handleQueries `catch` (\(SomeException _) -> putStrLn "Oops")
  serve "80" handleAdds `catch` (\(SomeException _) -> putStrLn "Oops")
