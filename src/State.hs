-- src/State.hs
module State where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Protocol (Nickname)
import System.IO (Handle)

-- Dùng Map để lưu trữ client, cho phép truy xuất nhanh bằng Nickname
type ClientMap = Map.Map Nickname Handle

-- TVar chứa ClientMap là trạng thái của server
type ServerState = TVar ClientMap

-- Hàm tạo một state mới (một TVar rỗng)
newServerState :: IO ServerState
newServerState = newTVarIO Map.empty

-- Thêm một client mới vào state.
-- Trả về Bool: True nếu thành công, False nếu tên đã tồn tồn tại.
addClient :: ServerState -> Nickname -> Handle -> STM Bool
addClient state nick handle = do
  clients <- readTVar state
  if Map.member nick clients
    then return False -- Tên đã tồn tại
    else do
      let newClients = Map.insert nick handle clients
      writeTVar state newClients
      return True -- Thêm thành công

-- Xóa một client khỏi state
removeClient :: ServerState -> Nickname -> STM ()
removeClient state nick = do
  clients <- readTVar state
  let newClients = Map.delete nick clients
  writeTVar state newClients

-- Lấy danh sách tất cả các Handle đang kết nối (để broadcast)
getAllHandles :: ServerState -> STM [Handle]
getAllHandles state = do
  clients <- readTVar state
  return $ Map.elems clients