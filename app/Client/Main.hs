-- app/Client/Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import Data.IORef
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Network.Socket hiding (recv)
import qualified Protocol as P
import System.Environment (getArgs)
import System.IO

chunkSize :: Int
chunkSize = 4096

main :: IO ()
main = withSocketsDo $ do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    [host, port] -> do
      putStrLn $ "Dang ket noi den " ++ host ++ ":" ++ port ++ "..."
      addr <- resolve host port
      bracket (open addr) close $ \sock -> do
        connect sock (addrAddress addr)
        putStrLn "OK. Da ket noi thanh cong."
        session sock
    _ -> putStrLn "Loi: Can cung cap dia chi va cong. Vi du: stack exec client-exe -- 0.tcp.ap.ngrok.io 19781"

session :: Socket -> IO ()
session sock =
  bracket (socketToHandle sock ReadWriteMode) hClose $ \handle -> do
    hSetBuffering handle LineBuffering

    pendingSendFileRef <- newIORef Nothing
    receivingFileRef <- newIORef Nothing

    putStrLn "Nhap ten cua ban:"
    nickname <- getLine
    hPutStrLn handle (P.serialize (P.Login nickname))

    putStrLn "--------------------------------------------------"
    putStrLn "Huong dan:"
    putStrLn " - De chat: Go tin nhan va nhan Enter."
    putStrLn " - De gui file: /send <ten_nguoi_nhan> <duong_dan_den_file>"
    putStrLn "--------------------------------------------------"

    void . forkIO $ listenLoop handle pendingSendFileRef receivingFileRef `catch` handleServerDisconnect
    sendLoop handle pendingSendFileRef `catch` handleInputError

listenLoop :: Handle -> IORef (Maybe FilePath) -> IORef (Maybe Handle) -> IO ()
listenLoop handle pendingSendFileRef receivingFileRef = forever $ do
  line <- hGetLine handle
  case P.parse line of
    Just (P.AcceptFile _) -> do
      putStrLn "[He thong]: Yeu cau duoc chap nhan. Bat dau gui file..."
      mFilepath <- readIORef pendingSendFileRef
      case mFilepath of
        Just filepath -> void . forkIO $ sendFile handle filepath
        Nothing -> putStrLn "[Loi]: Khong tim thay ten file de gui."
      writeIORef pendingSendFileRef Nothing

    -- SUA O DAY
    Just (P.ReceiveFileChunk chunk) -> do
      mFileHandle <- readIORef receivingFileRef
      case mFileHandle of
        Just fileHandle -> B.hPut fileHandle chunk
        Nothing -> return ()

    -- SUA O DAY
    Just P.ReceiveEndOfFile -> do
      putStrLn "[He thong]: Nhan file thanh cong."
      mFileHandle <- readIORef receivingFileRef
      case mFileHandle of
        Just fileHandle -> hClose fileHandle
        Nothing -> return ()
      writeIORef receivingFileRef Nothing

    Just msg -> displayMessage msg receivingFileRef
    Nothing -> return ()

sendLoop :: Handle -> IORef (Maybe FilePath) -> IO ()
sendLoop handle pendingSendFileRef = forever $ do
  userInput <- getLine
  case words userInput of
    ["/send", recipient, filepath] -> do
      putStrLn $ "[Ban]: Dang gui yeu cau gui file '" ++ filepath ++ "' den '" ++ recipient ++ "'..."
      writeIORef pendingSendFileRef (Just filepath)
      hPutStrLn handle (P.serialize (P.SendFileRequest recipient filepath))

    ["/accept", sender] -> do
      putStrLn $ "[Ban]: Da chap nhan nhan file tu '" ++ sender ++ "'."
      hPutStrLn handle (P.serialize (P.FileResponse sender True))

    ["/reject", sender] -> do
      putStrLn $ "[Ban]: Da tu choi nhan file tu '" ++ sender ++ "'."
      hPutStrLn handle (P.serialize (P.FileResponse sender False))

    _ -> hPutStrLn handle (P.serialize (P.PublicMessage userInput))

displayMessage :: P.ServerMessage -> IORef (Maybe Handle) -> IO ()
displayMessage (P.FileOffer fromNick filepath) receivingFileRef = do
  let receivePath = "nhan_" ++ fromNick ++ "_" ++ takeFileName filepath
  putStrLn $ "\n[Server]: Nguoi dung '" ++ fromNick ++ "' muon gui cho ban file '" ++ filepath ++ "'.\n" ++
             "         Neu dong y, file se duoc luu tai: " ++ receivePath ++ "\n" ++
             "         Go '/accept " ++ fromNick ++ "' de dong y, hoac '/reject " ++ fromNick ++ "' de tu choi."
  fileHandle <- openFile receivePath WriteMode
  hSetBinaryMode fileHandle True
  writeIORef receivingFileRef (Just fileHandle)
displayMessage (P.Broadcast nick msg) _ = putStrLn $ "[" ++ nick ++ "]: " ++ msg
displayMessage (P.UserJoined nick) _ = putStrLn $ "[" ++ nick ++ "] da tham gia phong chat."
displayMessage (P.UserLeft nick) _ = putStrLn $ "[" ++ nick ++ "] da roi khoi phong chat."
displayMessage (P.ServerInfo msg) _ = putStrLn $ "[Server]: " ++ msg
displayMessage _ _ = return ()

sendFile :: Handle -> FilePath -> IO ()
sendFile handle path =
  catch (bracket (openBinaryFile path ReadMode) hClose go) handleSendError
  where
    go fileHandle = do
      contents <- B.hGetContents fileHandle
      let chunks = splitUp contents
      mapM_ (sendChunk handle) chunks
      hPutStrLn handle (P.serialize P.EndOfFile)
      putStrLn "[He thong]: Gui file hoan tat."

    splitUp bs
      | B.null bs = []
      | otherwise = let (chunk, rest) = B.splitAt chunkSize bs in chunk : splitUp rest

    sendChunk h c = hPutStrLn h (P.serialize (P.FileChunk c))

    handleSendError :: SomeException -> IO ()
    handleSendError e = putStrLn $ "[Loi gui file]: " ++ show e

handleServerDisconnect :: SomeException -> IO ()
handleServerDisconnect _ = putStrLn "\nMat ket noi den server."

handleInputError :: SomeException -> IO ()
handleInputError _ = return ()

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints = defaultHints {addrSocketType = Stream}
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

open :: AddrInfo -> IO Socket
open addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

takeFileName :: FilePath -> FilePath
takeFileName = reverse . takeWhile (/= '\\') . reverse . takeWhile (/= '/') . reverse