-- app/Server/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally, bracket, catch, SomeException)
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
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (Connection, ServerApp, PendingConnection, acceptRequest, forkPingThread, receiveData, sendTextData, defaultConnectionOptions)
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Database.SQLite.Simple as SQLite

import Database
import Protocol
import State

-- Kieu du lieu cho request Dang nhap
data LoginRequest = LoginRequest
  { loginUser :: T.Text,
    loginPass :: T.Text
  } deriving (Show, Generic)
instance FromJSON LoginRequest

-- Kieu du lieu cho request Dang ky (CO THEM XAC NHAN)
data RegisterRequest = RegisterRequest
  { regUser :: T.Text,
    regPass :: T.Text,
    regPassConfirm :: T.Text
  } deriving (Show, Generic)
instance FromJSON RegisterRequest

-- Kieu du lieu cho tin nhan xac thuc WebSocket
data WsAuth = WsAuth { nickname :: Nickname }
  deriving (Show, Generic)
instance FromJSON WsAuth

-- Ham broadcast (gui tin nhan den tat ca moi nguoi)
broadcast :: ServerState -> ServerMessage -> IO ()
broadcast state msg = do
  clients <- atomically $ getAllClients state
  let encodedMsg = encode msg
  forM_ clients $ \(Client conn _) ->
    sendTextData conn encodedMsg

-- HAM MOI: Gui danh sach user cho tat ca
broadcastUserList :: ServerState -> IO ()
broadcastUserList state = do
  nicks <- atomically $ getAllNicknames state
  broadcast state (UserList nicks)

-- HAM MOI: Gui tin nhan rieng
sendPrivateMessage :: ServerState -> Nickname -> Nickname -> T.Text -> IO ()
sendPrivateMessage state fromNick toNick content = do
  mClient <- atomically $ getClientByNick state toNick
  case mClient of
    Just (Client conn _) -> sendTextData conn (encode (ReceivePrivateMessage fromNick content))
    Nothing -> putStrLn $ "Khong tim thay user " ++ T.unpack toNick

-- Ham xu ly logic chat cho tung client
webSocketApp :: ServerState -> SQLite.Connection -> ServerApp
webSocketApp state dbConn pending = do
  conn <- acceptRequest pending
  forkPingThread conn 30

  mNick <-
    ( do
        msg <- receiveData conn :: IO LBS.ByteString
        case eitherDecode msg of
          Right (WsAuth nick) -> do
            isOnline <- atomically $ Map.member nick <$> readTVar (clientState state)
            if isOnline
              then do
                sendTextData conn (encode (ServerInfo "Ten nay da co nguoi dang nhap."))
                return Nothing
              else return (Just nick)
          Left _ -> do
            sendTextData conn (encode (ServerInfo "Xac thuc that bai."))
            return Nothing
    )
      `catch` (\e -> const (return Nothing) (e :: SomeException))

  case mNick of
    Nothing -> return ()
    Just nick -> do
      let client = Client conn nick
      atomically $ addClient state client
      broadcast state (UserJoined nick)
      broadcastUserList state -- CAP NHAT DANH SACH USER
      putStrLn $ "Client da ket noi: " ++ T.unpack nick

      history <- getRecentMessages dbConn
      sendTextData conn (encode (LoadHistory (map (\(Message _ s c) -> Broadcast s c) (reverse history))))

      let loop = forever $ do
            jsonMsg <- receiveData conn
            case eitherDecode jsonMsg of
              Right (SendPublicMessage content) -> do
                liftIO $ saveMessage dbConn nick content
                broadcast state (Broadcast nick content)
              
              -- LOGIC MOI: Xu ly tin nhan rieng
              Right (SendPrivateMessage toNick content) -> do
                sendPrivateMessage state nick toNick content

              _ -> sendTextData conn (encode (ServerInfo "Tin nhan khong hop le"))

      let cleanup = do
            atomically $ removeClient state nick
            broadcast state (UserLeft nick)
            broadcastUserList state -- CAP NHAT DANH SACH USER
            putStrLn $ "Client da ngat ket noi: " ++ T.unpack nick
      
      loop `finally` cleanup

-- Ham main chinh, khoi dong Web Server
main :: IO ()
main = do
  putStrLn "Khoi dong server tren port 3000..."
  state <- newServerState
  dbConn <- initDB

  scottyAppInstance <- scottyApp $ do
    middleware simpleCors

    -- SUA LAI: Trang chu la trang dang nhap
    get "/" $ file "static/login.html"
    
    -- TRANG MOI: Trang dang ky
    get "/register" $ file "static/register.html"

    -- TRANG MOI: Trang chat (sau khi dang nhap)
    get "/chat" $ file "static/chat.html"
    
    -- Phuc vu cac file tinh khac
    get "/style.css" $ file "static/style.css"
    get "/auth.js" $ file "static/auth.js"
    get "/chat.js" $ file "static/chat.js"

    -- API cho Dang ky (CO KIEM TRA MAT KHAU)
    post "/register" $ do
      req <- jsonData :: ActionM RegisterRequest
      if regPass req /= regPassConfirm req
        then status status400 >> json ("{ \"status\": \"error\", \"message\": \"Mat khau xac nhan khong khop\" }" :: T.Text)
        else do
          success <- liftIO $ createUser dbConn (regUser req) (regPass req)
          if success
            then json ("{ \"status\": \"success\" }" :: T.Text)
            else status status400 >> json ("{ \"status\": \"error\", \"message\": \"Ten da ton tai\" }" :: T.Text)

    -- API cho Dang nhap
    post "/login" $ do
      req <- jsonData :: ActionM LoginRequest
      mNick <- liftIO $ validateUser dbConn (loginUser req) (loginPass req)
      case mNick of
        Just nick -> json $ object ["status" .= ("success" :: T.Text), "nickname" .= nick]
        Nothing -> status status401 >> json ("{ \"status\": \"error\", \"message\": \"Sai ten dang nhap hoac mat khau\" }" :: T.Text)
    
  putStrLn "Dang chay server tren port 3000..."
  run 3000 $ websocketsOr defaultConnectionOptions (webSocketApp state dbConn) scottyAppInstance