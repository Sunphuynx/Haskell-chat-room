-- src/Protocol.hs
module Protocol
  ( Nickname,
    ClientMessage (..),
    ServerMessage (..),
    serialize,
    parse,
  )
where

import Data.ByteString (ByteString)

type Nickname = String

-- Tin nhan Client gui den Server
data ClientMessage
  = Login Nickname
  | PublicMessage String
  | SendFileRequest Nickname FilePath
  | FileResponse Nickname Bool
  | FileChunk ByteString  -- Client gui di
  | EndOfFile             -- Client gui di
  deriving (Show, Read, Eq)

-- Tin nhan Server gui den Client
data ServerMessage
  = Broadcast Nickname String
  | UserJoined Nickname
  | UserLeft Nickname
  | ServerInfo String
  | FileOffer Nickname FilePath
  | AcceptFile Nickname
  | ReceiveFileChunk ByteString  -- Server gui den (TEN MOI)
  | ReceiveEndOfFile             -- Server gui den (TEN MOI)
  deriving (Show, Read, Eq)

serialize :: (Show a) => a -> String
serialize = show

parse :: (Read a) => String -> Maybe a
parse s = case reads s of
  [(msg, "")] -> Just msg
  _ -> Nothing