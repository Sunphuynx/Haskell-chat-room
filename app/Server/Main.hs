-- app/server/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (when)
import ParallelOps (countVowelsParallel)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally, bracket, SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, forM_)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, object, (.=))
import GHC.Generics (Generic)
import qualified Data.Map as Map
import Web.Scotty
import Network.HTTP.Types.Status (status200, status400, status401)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, ServerApp, PendingConnection, acceptRequest, forkPingThread, receiveData, sendTextData, defaultConnectionOptions)
import qualified Database.SQLite.Simple as SQLite
import System.Directory (createDirectoryIfMissing)
import Network.Wai.Parse (FileInfo, fileContent, fileName)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Database
import Protocol
import State

data LoginRequest = LoginRequest
  { loginUser :: T.Text,
    loginPass :: T.Text
  } deriving (Show, Generic)
instance FromJSON LoginRequest

data RegisterRequest = RegisterRequest
  { regUser :: T.Text,
    regPass :: T.Text,
    regPassConfirm :: T.Text
  } deriving (Show, Generic)
instance FromJSON RegisterRequest

data WsAuth = WsAuth { nickname :: Nickname }
  deriving (Show, Generic)
instance FromJSON WsAuth

broadcast :: ServerState -> ServerMessage -> IO ()
broadcast state msg = do
  clients <- atomically $ getAllClients state
  let encodedMsg = encode msg
  forM_ clients $ \(Client conn _) ->
    sendTextData conn encodedMsg

broadcastUserList :: ServerState -> IO ()
broadcastUserList state = do
  nicks <- atomically $ getAllNicknames state
  broadcast state (UserList nicks)

sendPrivateMessage :: ServerState -> Nickname -> Nickname -> T.Text -> IO ()
sendPrivateMessage state fromNick toNick content = do
  mClient <- atomically $ getClientByNick state toNick
  case mClient of
    Just (Client conn _) -> sendTextData conn (encode (ReceivePrivateMessage fromNick content))
    Nothing -> putStrLn $ "Không tìm thấy user " ++ T.unpack toNick

webSocketApp :: ServerState -> SQLite.Connection -> ServerApp
webSocketApp state dbConn pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30

  mNick <-
    ( do
        msg <- receiveData conn :: IO LBS.ByteString
        case eitherDecode msg of
          Right (WsAuth nick) -> do
            isOnline <- atomically $ Map.member nick <$> readTVar state
            if isOnline
              then do
                sendTextData conn (encode (ServerInfo "Tên này đã có người đăng nhập."))
                return Nothing
              else return (Just nick)
          Left _ -> do
            sendTextData conn (encode (ServerInfo "Xác thực thất bại."))
            return Nothing
    )
      `catch` (\e -> const (return Nothing) (e :: SomeException))

  case mNick of
    Nothing -> return ()
    Just nick -> do
      let client = Client conn nick
      atomically $ addClient state client
      broadcast state (UserJoined nick)
      broadcastUserList state
      putStrLn $ "Client da ket noi: " ++ T.unpack nick

      history <- getRecentMessages dbConn
      sendTextData conn (encode (LoadHistory (map (\(Message _ s c) -> Broadcast s c) (reverse history))))

      let loop = forever $ do
            jsonMsg <- receiveData conn
            case eitherDecode jsonMsg of
              Right (SendPublicMessage content) -> do
                liftIO $ saveMessage dbConn nick content
                broadcast state (Broadcast nick content)
              
              Right (SendPrivateMessage toNick content) -> do
                sendPrivateMessage state nick toNick content

              _ -> sendTextData conn (encode (ServerInfo "Tin nhắn không hợp lệ"))

      let cleanup = do
            atomically $ removeClient state nick
            broadcast state (UserLeft nick)
            broadcastUserList state
            putStrLn $ "Client da ngat ket noi: " ++ T.unpack nick
      
      loop `finally` cleanup

main :: IO ()
main = do
  putStrLn "Khoi dong server tren port 3000..."
  createDirectoryIfMissing True "uploads"
  
  state <- newServerState
  dbConn <- initDB

  scottyAppInstance <- scottyApp $ do
    middleware simpleCors

    get "/" $ file "static/login.html"
    get "/register" $ file "static/register.html"
    get "/chat" $ file "static/chat.html"
    get "/style.css" $ file "static/style.css"
    get "/auth.js" $ file "static/auth.js"
    get "/chat.js" $ file "static/chat.js"

    post "/register" $ do
      req <- jsonData :: ActionM RegisterRequest
      if regPass req /= regPassConfirm req
        then status status400 >> json ("{ \"status\": \"error\", \"message\": \"Mật khẩu xác nhận không khớp\" }" :: T.Text)
        else do
          success <- liftIO $ createUser dbConn (regUser req) (regPass req)
          if success
            then json ("{ \"status\": \"success\" }" :: T.Text)
            else status status400 >> json ("{ \"status\": \"error\", \"message\": \"Tên đã tồn tại\" }" :: T.Text)

    post "/login" $ do
      req <- jsonData :: ActionM LoginRequest
      mNick <- liftIO $ validateUser dbConn (loginUser req) (loginPass req)
      case mNick of
        Just nick -> json $ object ["status" .= ("success" :: T.Text), "nickname" .= nick]
        Nothing -> status status401 >> json ("{ \"status\": \"error\", \"message\": \"Sai tên đăng nhập hoặc mật khẩu\" }" :: T.Text)
    
    post "/upload" $ do
      fs <- files
      case fs of
        [("file", fileInfo)] -> do
          let filename_bs = fileName fileInfo
          let filecontent = fileContent fileInfo
          let filename = decodeUtf8 filename_bs
          let newPath = "uploads/" ++ T.unpack filename
          
          liftIO $ LBS.writeFile newPath filecontent

          nick <- param "nickname" :: ActionM Nickname
          let fileUrl = "/files/" <> filename 

          liftIO $ broadcast state (FileBroadcast nick filename fileUrl)

          when (T.isSuffixOf ".txt" filename) $ do
            let vowelCount = countVowelsParallel filecontent
            let infoMsg = "File '" <> filename <> "' có " <> T.pack (show vowelCount) <> " nguyên âm (đã kiểm tra song song)."
            liftIO $ broadcast state (ServerInfo infoMsg)

          json ("{ \"status\": \"success\" }" :: T.Text)
          
        _ -> do
          status status400
          json ("{ \"status\": \"error\", \"message\": \"Không có file nào được tải lên\" }" :: T.Text)
          
    get "/files/:filename" $ do
      filename <- param "filename"
      file $ "uploads/" ++ filename
    
  putStrLn "Dang chay server tren port 3000..."
  run 3000 $ websocketsOr defaultConnectionOptions (webSocketApp state dbConn) scottyAppInstance