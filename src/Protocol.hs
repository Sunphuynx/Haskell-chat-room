-- src/Protocol.hs
module Protocol
  ( Nickname,
    ClientMessage (..),
    ServerMessage (..),
    serialize,
    parse,
  )
where

-- Kiểu dữ liệu cho tên người dùng
type Nickname = String

-- Các loại tin nhắn Client có thể gửi đến Server
-- Dùng `deriving (Show, Read)` để dễ dàng chuyển đổi sang String và ngược lại
data ClientMessage
  = Login Nickname -- Tin nhắn đăng nhập với tên người dùng
  | PublicMessage String -- Tin nhắn công khai gửi cho mọi người
  deriving (Show, Read, Eq)

-- Các loại tin nhắn Server có thể gửi đến Client
data ServerMessage
  = Broadcast Nickname String -- Tin nhắn broadcast từ một người dùng
  | UserJoined Nickname -- Thông báo có người mới tham gia
  | UserLeft Nickname -- Thông báo có người rời đi
  | ServerInfo String -- Tin nhắn thông báo từ hệ thống server
  deriving (Show, Read, Eq)

-- Hàm chuyển một đối tượng (Message) thành chuỗi để gửi đi
-- `show` sẽ chuyển nó thành một chuỗi mà Haskell có thể đọc lại được
-- Ví dụ: Login "Tam" -> "Login \"Tam\""
serialize :: (Show a) => a -> String
serialize = show

-- Hàm phân tích một chuỗi nhận được thành một đối tượng (Message)
-- `reads` an toàn hơn `read`, nó trả về một danh sách các kết quả có thể có
-- `case ... of` để xử lý trường hợp parse thành công hoặc thất bại (trả về Nothing)
parse :: (Read a) => String -> Maybe a
parse s = case reads s of
  -- Chỉ chấp nhận nếu parse thành công và không còn ký tự thừa
  [(msg, "")] -> Just msg
  -- Các trường hợp khác đều là parse lỗi
  _ -> Nothing