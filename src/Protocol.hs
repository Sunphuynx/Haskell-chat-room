-- src/Protocol.hs
{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type Nickname = Text

-- Tin nhan Client gui den Server (qua WebSocket)
data ClientMessage
  = SendPublicMessage Text
  | SendPrivateMessage Nickname Text -- Gui tin nhan rieng cho (NguoiNhan) (NoiDung)
  deriving (Show, Generic)

instance FromJSON ClientMessage

-- Tin nhan Server gui den Client (qua WebSocket)
data ServerMessage
  = Broadcast Nickname Text
  | ReceivePrivateMessage Nickname Text -- Nhan tin nhan rieng tu (NguoiGui) (NoiDung)
  | UserJoined Nickname
  | UserLeft Nickname
  | UserList [Nickname] -- Danh sach toan bo user dang online
  | ServerInfo Text
  | LoadHistory [ServerMessage]
  deriving (Show, Generic)

instance ToJSON ServerMessage