-- app/Server/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void)
import GHC.IO.Encoding (setLocaleEncoding)
import Network.Socket
import qualified Protocol as P -- SỬA Ở ĐÂY: Dùng qualified import
import State
import System.IO

main :: IO ()
main = withSocketsDo $ do
  setLocaleEncoding utf8
  state <- newServerState
  addr <- resolve "3000"
  sock <- open addr
  bind sock (addrAddress addr)
  listen sock 10
  putStrLn "Server dang lang nghe tren port 3000..."
  forever $ do
    (conn, peer) <- accept sock
    putStrLn $ "Ket noi moi tu: " ++ show peer
    void . forkIO $ handleClient conn state

handleClient :: Socket -> ServerState -> IO ()
handleClient sock state = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering
  conversationLoop handle state `finally` hClose handle

conversationLoop :: Handle -> ServerState -> IO ()
conversationLoop handle state = do
  hPutStrLn handle (P.serialize (P.ServerInfo "Chao mung! Vui long nhap ten cua ban."))
  line <- hGetLine handle
  case P.parse line of -- SỬA Ở ĐÂY
    Just (P.Login nick) -> do -- SỬA Ở ĐÂY
      success <- atomically $ addClient state nick handle
      if success
        then do
          broadcast state (P.UserJoined nick) -- SỬA Ở ĐÂY
          putStrLn $ "+ " ++ nick ++ " da tham gia."
          talkLoop handle nick state `catch` handleException nick
        else do
          hPutStrLn handle (P.serialize (P.ServerInfo $ "Ten '" ++ nick ++ "' da duoc su dung. Vui long thu lai.")) -- SỬA Ở ĐÂY
          hClose handle
    _ -> hPutStrLn handle (P.serialize (P.ServerInfo "Giao thuc khong hop le. Can gui tin nhan Login truoc.")) -- SỬA Ở ĐÂY
  where
    handleException :: P.Nickname -> SomeException -> IO ()
    handleException nick _ = do
      atomically $ removeClient state nick
      broadcast state (P.UserLeft nick) -- SỬA Ở ĐÂY
      putStrLn $ "- " ++ nick ++ " da roi khoi phong chat."

talkLoop :: Handle -> P.Nickname -> ServerState -> IO ()
talkLoop handle nick state = forever $ do
  line <- hGetLine handle
  case P.parse line of -- SỬA Ở ĐÂY
    Just (P.PublicMessage msg) -> do -- SỬA Ở ĐÂY
      broadcast state (P.Broadcast nick msg) -- SỬA Ở ĐÂY
    _ -> hPutStrLn handle (P.serialize (P.ServerInfo "Tin nhan khong hop le.")) -- SỬA Ở ĐÂY

broadcast :: ServerState -> P.ServerMessage -> IO ()
broadcast state msg = do
  handles <- atomically $ getAllHandles state
  let msgStr = P.serialize msg -- SỬA Ở ĐÂY
  mapM_ (\h -> hPutStrLn h msgStr `catch` (\e -> const (return ()) (e :: SomeException))) handles

-- Helper functions (giữ nguyên)
resolve :: String -> IO AddrInfo
resolve port = do
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) Nothing (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)