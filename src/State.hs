-- src/State.hs
module State where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Protocol (Nickname)
import System.IO (Handle)

-- Luu thong tin client
type ClientMap = Map.Map Nickname Handle

-- Luu trang thai phien truyen file: NguoiGui -> NguoiNhan
type TransferMap = Map.Map Nickname Nickname

-- Kieu du lieu moi cho trang thai cua server
data ServerState = ServerState
  { clientState :: TVar ClientMap,
    transferState :: TVar TransferMap
  }

-- Ham tao state moi
newServerState :: IO ServerState
newServerState = do
  clients <- newTVarIO Map.empty
  transfers <- newTVarIO Map.empty
  return $ ServerState clients transfers

-- Them client moi
addClient :: ServerState -> Nickname -> Handle -> STM Bool
addClient state nick handle = do
  clients <- readTVar (clientState state)
  if Map.member nick clients
    then return False
    else do
      let newClients = Map.insert nick handle clients
      writeTVar (clientState state) newClients
      return True

-- Xoa client
removeClient :: ServerState -> Nickname -> STM ()
removeClient state nick =
  modifyTVar' (clientState state) (Map.delete nick)

-- Lay tat ca handle
getAllHandles :: ServerState -> STM [Handle]
getAllHandles state = Map.elems <$> readTVar (clientState state)

-- Lay handle cua mot nguoi dung cu the
getHandleByNick :: ServerState -> Nickname -> STM (Maybe Handle)
getHandleByNick state nick = Map.lookup nick <$> readTVar (clientState state)

-- Bat dau mot phien truyen file
startTransfer :: ServerState -> Nickname -> Nickname -> STM ()
startTransfer state sender receiver =
  modifyTVar' (transferState state) (Map.insert sender receiver)

-- Ket thuc mot phien truyen file
endTransfer :: ServerState -> Nickname -> STM ()
endTransfer state sender =
  modifyTVar' (transferState state) (Map.delete sender)

-- Tim nguoi nhan trong mot phien truyen file
getTransferRecipient :: ServerState -> Nickname -> STM (Maybe Nickname)
getTransferRecipient state sender = Map.lookup sender <$> readTVar (transferState state)