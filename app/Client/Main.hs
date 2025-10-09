-- app/client/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import GHC.IO.Encoding (setLocaleEncoding)
import Network.Socket
import qualified Protocol as P -- SỬA Ở ĐÂY: Dùng qualified import
import System.IO

main :: IO ()
main = withSocketsDo $ do
  setLocaleEncoding utf8
  addr <- resolve "127.0.0.1" "3000"
  sock <- open addr
  putStrLn "Dang ket noi den server..."
  connect sock (addrAddress addr)
  putStrLn "Da ket noi thanh cong."
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  session handle

session :: Handle -> IO ()
session handle = do
  -- Bắt đầu bằng việc yêu cầu người dùng nhập tên
  putStrLn "Nhap ten cua ban:"
  nickname <- getLine
  -- Gửi tin nhắn Login theo đúng giao thức
  hPutStrLn handle (P.serialize (P.Login nickname))

  -- Luồng lắng nghe tin nhắn từ server
  void . forkIO $ listenLoop `catch` handleServerDisconnect
  -- Luồng chính để gửi tin nhắn
  sendLoop `catch` handleInputError
  where
    listenLoop = forever $ do
      line <- hGetLine handle
      case P.parse line of -- SỬA Ở ĐÂY
        Just msg -> displayMessage msg
        Nothing -> putStrLn " Nhan duoc tin nhan khong the phan tich tu server."

    sendLoop = forever $ do
      msg <- getLine
      -- Gửi tin nhắn công khai theo đúng giao thức
      hPutStrLn handle (P.serialize (P.PublicMessage msg)) -- SỬA Ở ĐÂY

    displayMessage :: P.ServerMessage -> IO () -- SỬA Ở ĐÂY
    displayMessage (P.Broadcast nick msg) = putStrLn $ "[" ++ nick ++ "]: " ++ msg -- SỬA Ở ĐÂY
    displayMessage (P.UserJoined nick) = putStrLn $ " [" ++ nick ++ "] da tham gia phong chat." -- SỬA Ở ĐÂY
    displayMessage (P.UserLeft nick) = putStrLn $ " [" ++ nick ++ "] da roi khoi phong chat." -- SỬA Ở ĐÂY
    displayMessage (P.ServerInfo msg) = putStrLn $ "[Server]: " ++ msg -- SỬA Ở ĐÂY

    handleServerDisconnect :: SomeException -> IO ()
    handleServerDisconnect _ = putStrLn "Mat ket noi den server."

    handleInputError :: SomeException -> IO ()
    handleInputError _ = putStrLn "Tam biet!"

-- Helper functions
resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints = defaultHints {addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)