-- src/State.hs
module State where

import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Text (Text)
import Network.WebSockets (Connection)
import Protocol (Nickname) -- Đảm bảo đã import Protocol

-- Thông tin của một client đang kết nối
data Client = Client
  { clientConn :: Connection,
    clientNick :: Nickname
  }

-- Kiểu dữ liệu Map để lưu Client
type ClientMap = Map.Map Nickname Client

-- **ĐÂY LÀ THAY ĐỔI QUAN TRỌNG**
-- ServerState bây giờ là một 'data' record, có 2 trường (field)
data ServerState = ServerState
  { clientState :: TVar ClientMap,      -- Trường để lưu danh sách client
    transferState :: TVar (Map.Map Nickname Nickname) -- Trường này để dành cho việc gửi file sau
  }

-- Hàm tạo state mới
newServerState :: IO ServerState
newServerState = do
  clients <- newTVarIO Map.empty
  transfers <- newTVarIO Map.empty
  return $ ServerState clients transfers -- Trả về một record

-- Thêm client (sử dụng record accessor 'clientState')
addClient :: ServerState -> Client -> STM ()
addClient state client =
  modifyTVar' (clientState state) (Map.insert (clientNick client) client)

-- Xóa client
removeClient :: ServerState -> Nickname -> STM ()
removeClient state nick =
  modifyTVar' (clientState state) (Map.delete nick)

-- Lấy tất cả client
getAllClients :: ServerState -> STM [Client]
getAllClients state = Map.elems <$> readTVar (clientState state)

-- Lấy tất cả nickname
getAllNicknames :: ServerState -> STM [Nickname]
getAllNicknames state = Map.keys <$> readTVar (clientState state)

-- Lấy một client theo tên
getClientByNick :: ServerState -> Nickname -> STM (Maybe Client)
getClientByNick state nick = Map.lookup nick <$> readTVar (clientState state)