-- src/Protocol.hs
module Protocol
  ( -- * Types
    Nickname,
    ClientMessage (..),
    ServerMessage (..),

    -- * Serialization
    serialize,
    parse,
  )
where

import Data.ByteString (ByteString)

-- | Kiểu dữ liệu cho tên người dùng.
type Nickname = String

-- | Các loại tin nhắn mà Client có thể gửi đến Server.
-- Dùng `deriving (Show, Read, Eq)` để dễ dàng chuyển đổi sang String và ngược lại.
data ClientMessage
  = -- | Tin nhắn đăng nhập khi client mới kết nối.
    Login Nickname
  | -- | Tin nhắn công khai gửi cho tất cả mọi người trong phòng chat.
    PublicMessage String
  | -- | Yêu cầu gửi một file cho một người dùng cụ thể.
    SendFileRequest Nickname FilePath
  | -- | Phản hồi của người nhận về yêu cầu nhận file.
    FileResponse Nickname Bool -- True = chấp nhận, False = từ chối
  | -- | Một mẩu dữ liệu (chunk) của file đang được gửi.
    FileChunk ByteString
  | -- | Tín hiệu báo rằng đã gửi xong toàn bộ file.
    EndOfFile
  deriving (Show, Read, Eq)

-- | Các loại tin nhắn mà Server có thể gửi đến Client.
data ServerMessage
  = -- | Tin nhắn được gửi lại cho tất cả client.
    Broadcast Nickname String
  | -- | Thông báo có người dùng mới tham gia.
    UserJoined Nickname
  | -- | Thông báo có người dùng đã rời đi.
    UserLeft Nickname
  | -- | Tin nhắn thông báo chung từ hệ thống server.
    ServerInfo String
  | -- | Server chuyển tiếp yêu cầu nhận file từ người gửi đến người nhận.
    FileOffer Nickname FilePath
  deriving (Show, Read, Eq)

-- | Chuyển một đối tượng (Message) thành chuỗi để gửi đi qua mạng.
-- `show` sẽ chuyển nó thành một chuỗi mà Haskell có thể đọc lại được.
-- Ví dụ: Login "Tam" -> "Login \"Tam\""
serialize :: (Show a) => a -> String
serialize = show

-- | Phân tích một chuỗi nhận được từ mạng thành một đối tượng (Message).
-- `reads` an toàn hơn `read`, nó trả về một danh sách các kết quả có thể có.
-- `case ... of` để xử lý trường hợp parse thành công hoặc thất bại.
parse :: (Read a) => String -> Maybe a
parse s = case reads s of
  -- Chỉ chấp nhận nếu parse thành công và không còn ký tự thừa.
  [(msg, "")] -> Just msg
  -- Các trường hợp khác đều coi là parse lỗi.
  _ -> Nothing