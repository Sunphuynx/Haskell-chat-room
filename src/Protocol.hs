{-# LANGUAGE DeriveGeneric #-}

module Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type Nickname = Text

data ClientMessage
  = SendPublicMessage Text
  | SendPrivateMessage Nickname Text
  deriving (Show, Generic)

instance FromJSON ClientMessage

data ServerMessage
  = Broadcast Nickname Text
  | ReceivePrivateMessage Nickname Text
  | UserJoined Nickname
  | UserLeft Nickname
  | UserList [Nickname]
  | ServerInfo Text
  | LoadHistory [ServerMessage]
  | FileBroadcast Nickname Text Text
  deriving (Show, Generic)

instance ToJSON ServerMessage