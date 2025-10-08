{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket
import System.IO
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = withSocketsDo $ do
    setLocaleEncoding utf8
    addr <- resolve "127.0.0.1" "3000"
    sock <- open addr
    connect sock (addrAddress addr)
    putStrLn "Da ket noi den server."

    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering

    -- Luồng lắng nghe tin nhắn từ server
    forkIO $ forever $ do
        serverMsg <- hGetLine handle
        putStrLn $ "Server: " ++ serverMsg

    -- Luồng nhập tin nhắn từ người dùng
    forever $ do
        msg <- getLine
        hPutStrLn handle msg

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
    let hints = defaultHints { addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
