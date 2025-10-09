-- app/client/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import GHC.IO.Encoding (setLocaleEncoding)
import Network.Socket
import qualified Protocol as P
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
  putStrLn "Nhap ten cua ban:"
  nickname <- getLine
  hPutStrLn handle (P.serialize (P.Login nickname))

  void . forkIO $ listenLoop `catch` handleServerDisconnect
  sendLoop `catch` handleInputError
  where
    listenLoop = forever $ do
      line <- hGetLine handle
      case P.parse line of
        Just msg -> displayMessage msg
        Nothing -> putStrLn " Nhan duoc tin nhan khong the phan tich tu server."

    sendLoop = forever $ do
      msg <- getLine
      -- TODO: Bổ sung logic để parse lệnh /send, /accept...
      hPutStrLn handle (P.serialize (P.PublicMessage msg))

    -- SỬA LỖI Ở ĐÂY: Thêm pattern cho FileOffer
    displayMessage :: P.ServerMessage -> IO ()
    displayMessage (P.Broadcast nick msg) = putStrLn $ "[" ++ nick ++ "]: " ++ msg
    displayMessage (P.UserJoined nick) = putStrLn $ " [" ++ nick ++ "] da tham gia phong chat."
    displayMessage (P.UserLeft nick) = putStrLn $ " [" ++ nick ++ "] da roi khoi phong chat."
    displayMessage (P.ServerInfo msg) = putStrLn $ "[Server]: " ++ msg
    displayMessage (P.FileOffer fromNick filepath) = -- <-- DÒNG MỚI ĐƯỢC THÊM VÀO
      putStrLn $ "[Server]: Nguoi dung '" ++ fromNick ++ "' muon gui cho ban file '" ++ filepath ++ "'. Go '/accept " ++ fromNick ++ "' de dong y."

    handleServerDisconnect :: SomeException -> IO ()
    handleServerDisconnect _ = putStrLn "Mat ket noi den server."

    handleInputError :: SomeException -> IO ()
    handleInputError _ = putStrLn "👋 Tạm biệt!"

-- Helper functions
resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints = defaultHints {addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)