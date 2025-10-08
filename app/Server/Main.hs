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
    addr <- resolve "3000"
    sock <- open addr
    bind sock (addrAddress addr)
    listen sock 10
    putStrLn " Server dang lang nghe tren port 3000..."

    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "ket noi moi tu: " ++ show peer
        forkIO $ handleClient conn

handleClient :: Socket -> IO ()
handleClient sock = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    hPutStrLn handle "Xin chao! Ban da ket noi den server."
    forever $ do
        msg <- hGetLine handle
        putStrLn $ " Nhan: " ++ msg
        hPutStrLn handle $ "Server echo: " ++ msg

resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) Nothing (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
