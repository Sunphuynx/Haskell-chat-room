-- app/Server/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever, void, when)
import qualified Data.Map as Map
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Socket
import qualified Protocol as P
import State
import System.IO

main :: IO ()
main = withSocketsDo $ do
  setLocaleEncoding utf8
  state <- newServerState
  addr <- resolve "3000"
  bracket (open addr) close $ \sock -> do
    bind sock (addrAddress addr)
    listen sock 10
    putStrLn "OK. Server dang lang nghe tren port 3000..."
    forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Ket noi moi tu: " ++ show peer
      void . forkIO $ handleClient conn state

handleClient :: Socket -> ServerState -> IO ()
handleClient sock state =
  bracket (socketToHandle sock ReadWriteMode) hClose $ \handle -> do
    hSetBuffering handle LineBuffering
    loginLoop handle state `catch` handleAnyException
  where
    handleAnyException :: SomeException -> IO ()
    handleAnyException _ = return ()

loginLoop :: Handle -> ServerState -> IO ()
loginLoop handle state = do
  hPutStrLn handle (P.serialize (P.ServerInfo "Chao mung. Vui long nhap ten cua ban."))
  line <- hGetLine handle
  case P.parse line of
    Just (P.Login nick) -> do
      success <- atomically $ addClient state nick handle
      if success
        then do
          broadcast state (P.UserJoined nick)
          putStrLn $ "OK. " ++ nick ++ " da tham gia."
          talkLoop handle nick state `catch` handleDisconnect nick
        else do
          hPutStrLn handle (P.serialize (P.ServerInfo $ "Ten '" ++ nick ++ "' da duoc su dung."))
    _ -> hPutStrLn handle (P.serialize (P.ServerInfo "Giao thuc khong hop le."))
  where
    handleDisconnect :: P.Nickname -> SomeException -> IO ()
    handleDisconnect nick _ = do
      atomically $ do
        removeClient state nick
        endTransfer state nick
      broadcast state (P.UserLeft nick)
      putStrLn $ "Client " ++ nick ++ " da roi khoi phong chat."

talkLoop :: Handle -> P.Nickname -> ServerState -> IO ()
talkLoop handle nick state = forever $ do
  line <- hGetLine handle
  case P.parse line of
    Just (P.PublicMessage msg) ->
      broadcast state (P.Broadcast nick msg)

    Just (P.SendFileRequest recipient filepath) -> do
      mRecipientHandle <- atomically $ getHandleByNick state recipient
      case mRecipientHandle of
        Just recipientHandle -> do
          hPutStrLn recipientHandle (P.serialize (P.FileOffer nick filepath))
        Nothing ->
          hPutStrLn handle (P.serialize (P.ServerInfo $ "Loi: Khong tim thay nguoi dung '" ++ recipient ++ "'."))

    Just (P.FileResponse sender approved) -> do
      mSenderHandle <- atomically $ getHandleByNick state sender
      case mSenderHandle of
        Just senderHandle ->
          when approved $ do
            atomically $ startTransfer state sender nick
            hPutStrLn senderHandle (P.serialize (P.AcceptFile nick))
        Nothing -> return ()

    Just (P.FileChunk chunk) -> do
      mRecipientNick <- atomically $ getTransferRecipient state nick
      case mRecipientNick of
        Just recipientNick -> forwardTo recipientNick (P.ReceiveFileChunk chunk) -- SUA O DAY
        Nothing -> return ()

    Just P.EndOfFile -> do
      mRecipientNick <- atomically $ getTransferRecipient state nick
      case mRecipientNick of
        Just recipientNick -> do
          forwardTo recipientNick P.ReceiveEndOfFile -- SUA O DAY
          atomically $ endTransfer state nick
        Nothing -> return ()

    _ -> return ()
  where
    forwardTo :: P.Nickname -> P.ServerMessage -> IO ()
    forwardTo recipientNick msg = do
      mRecipientHandle <- atomically $ getHandleByNick state recipientNick
      case mRecipientHandle of
        Just h -> hPutStrLn h (P.serialize msg) `catch` (\e -> const (return ()) (e :: SomeException))
        Nothing -> return ()

broadcast :: ServerState -> P.ServerMessage -> IO ()
broadcast state msg = do
  handles <- atomically $ getAllHandles state
  let msgStr = P.serialize msg
  mapM_ (\h -> hPutStrLn h msgStr `catch` (\e -> const (return ()) (e :: SomeException))) handles

resolve :: String -> IO AddrInfo
resolve port = do
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) Nothing (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)