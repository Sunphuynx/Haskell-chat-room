module State where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Text (Text)
import Network.WebSockets (Connection)
import Protocol (Nickname)

data Client = Client
  { clientConn :: Connection,
    clientNick :: Nickname
  }

type ClientMap = Map.Map Nickname Client
type ServerState = TVar ClientMap

newServerState :: IO ServerState
newServerState = newTVarIO Map.empty

addClient :: ServerState -> Client -> STM ()
addClient state client =
  modifyTVar' state (Map.insert (clientNick client) client)

removeClient :: ServerState -> Nickname -> STM ()
removeClient state nick =
  modifyTVar' state (Map.delete nick)

getAllClients :: ServerState -> STM [Client]
getAllClients state = Map.elems <$> readTVar state

getAllNicknames :: ServerState -> STM [Nickname]
getAllNicknames state = Map.keys <$> readTVar state

getClientByNick :: ServerState -> Nickname -> STM (Maybe Client)
getClientByNick state nick = Map.lookup nick <$> readTVar state