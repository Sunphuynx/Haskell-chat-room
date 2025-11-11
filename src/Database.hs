-- src/Database.hs
{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Applicative ((<|>))
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA256, Parameters(..))
import Crypto.Random.Types (MonadRandom(getRandomBytes))
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.SQLite.Simple

-- Dinh nghia kieu du lieu
data User = User
  { userId :: Int,
    username :: T.Text,
    passwordHash :: ByteString,
    userSalt :: ByteString
  }
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

data Message = Message
  { msgId :: Int,
    msgSender :: T.Text,
    msgContent :: T.Text
  }
instance FromRow Message where
  fromRow = Message <$> field <*> field <*> field

-- Ham khoi tao database
initDB :: IO Connection
initDB = do
  conn <- open "chat.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT UNIQUE NOT NULL, password_hash BLOB NOT NULL, salt BLOB NOT NULL)"
  execute_ conn "CREATE TABLE IF NOT EXISTS messages (id INTEGER PRIMARY KEY AUTOINCREMENT, sender TEXT NOT NULL, content TEXT NOT NULL, timestamp DATETIME DEFAULT CURRENT_TIMESTAMP)"
  return conn

-- Tao mot "salt" ngau nhien cho moi user
createSalt :: IO ByteString
createSalt = getRandomBytes 32

-- Bam mat khau voi salt
hashPassword :: T.Text -> ByteString -> ByteString
hashPassword pass salt =
  fastPBKDF2_SHA256 params (encodeUtf8 pass) salt
  where params = Parameters { iterCounts = 100000, outputLength = 32 }

-- Ham tao user moi
-- Tra ve True neu thanh cong, False neu ten da ton tai
createUser :: Connection -> T.Text -> T.Text -> IO Bool
createUser conn user pass = do
  mUser <- findUser conn user
  case mUser of
    Just _ -> return False -- User da ton tai
    Nothing -> do
      salt <- createSalt
      let hashedPass = hashPassword pass salt
      execute conn "INSERT INTO users (username, password_hash, salt) VALUES (?, ?, ?)" (user, hashedPass, salt)
      return True

-- Ham tim user theo ten
findUser :: Connection -> T.Text -> IO (Maybe User)
findUser conn user = do
  results <- query conn "SELECT id, username, password_hash, salt FROM users WHERE username = ?" (Only user)
  case results of
    [] -> return Nothing
    (u:_) -> return (Just u)

-- Ham xac thuc user
validateUser :: Connection -> T.Text -> T.Text -> IO (Maybe T.Text)
validateUser conn user pass = do
  mUser <- findUser conn user
  case mUser of
    Nothing -> return Nothing
    Just (User _ uname dbHash dbSalt) ->
      if hashPassword pass dbSalt == dbHash
        then return (Just uname) -- Tra ve Nickname (hien tai la username)
        else return Nothing

-- Luu tin nhan
saveMessage :: Connection -> T.Text -> T.Text -> IO ()
saveMessage conn sender content =
  execute conn "INSERT INTO messages (sender, content) VALUES (?, ?)" (sender, content)

-- Lay 100 tin nhan gan nhat
getRecentMessages :: Connection -> IO [Message]
getRecentMessages conn = do
  query_ conn "SELECT id, sender, content FROM messages ORDER BY timestamp DESC LIMIT 100"