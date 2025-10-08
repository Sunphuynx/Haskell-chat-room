# 💬 Haskell Chat Room

Ứng dụng **Chat Room** viết bằng **Haskell** sử dụng thư viện `network`, `async`, và `stm`, cho phép client và server giao tiếp qua TCP socket.  
Dự án được phát triển trong khuôn khổ môn học **Lập Trình Hàm – Cuối kỳ**.

---

## 🧑‍💻 Thành viên nhóm

| Vai trò | Họ và tên | Nhiệm vụ |
|----------|------------|----------|
| **TV1** | **Phạm Duy Hoàng** | Phát triển phần **Server**: quản lý kết nối, gửi/broadcast tin nhắn đến nhiều client |
| **TV2** | **Phùng Chí Tâm** | Thiết lập dự án, xây dựng **Client** & **Protocol**, kiểm thử và cấu hình hệ thống |

---

## 🧱 Cấu trúc dự án

haskell-chat-room/
├── app/
│ ├── client/
│ │ └── Main.hs # Code chính của client
│ └── server/
│ └── Main.hs # Code chính của server
├── src/
│ ├── Protocol.hs # Định nghĩa giao thức (TV2 phụ trách)
│ ├── State.hs # Quản lý trạng thái server (TV1 phụ trách)
│ └── Lib.hs # Các hàm dùng chung
├── package.yaml # Thông tin package, dependency
├── stack.yaml # Cấu hình Stack
├── README.md # Tài liệu mô tả dự án
└── .gitignore

yaml
Sao chép mã

---

## ⚙️ Môi trường & Công cụ

- **Ngôn ngữ:** Haskell
- **Compiler:** GHC 9.6.5
- **Build tool:** Stack
- **IDE khuyến nghị:** VS Code + Haskell Language Server
- **Hệ điều hành:** Windows / Linux đều chạy được

---

## 🚀 Hướng dẫn cài đặt & chạy

### 1️⃣ Cài đặt Stack (nếu chưa có)

👉 [Tải Stack tại đây](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Kiểm tra sau khi cài:
```bash
stack --version
2️⃣ Clone dự án
bash
Sao chép mã
git clone https://github.com/Sunphuynx/Haskell-chat-room.git
cd Haskell-chat-room
3️⃣ Biên dịch dự án
bash
Sao chép mã
stack build
Nếu thấy dòng Successfully built haskell-chat-room-0.1.0.0 → ✅ OK

4️⃣ Chạy chương trình
🔹 Mở 2 cửa sổ terminal riêng biệt:
Cửa sổ 1 (Server):

bash
Sao chép mã
stack exec server-exe
Cửa sổ 2 (Client):

bash
Sao chép mã
stack exec client-exe
5️⃣ Kiểm tra hoạt động
Khi client kết nối thành công, bạn sẽ thấy:

csharp
Sao chép mã
[OK] Da ket noi toi server.
Gõ tin nhắn bên client:

nginx
Sao chép mã
xin chao
Server sẽ hiển thị:

arduino
Sao chép mã
Client: xin chao
Và phản hồi về client:

yaml
Sao chép mã
Nhan duoc: xin chao
🧠 Mục tiêu chức năng cuối kỳ
Chức năng	Mô tả	Trạng thái
Cấu hình Stack, thư mục chuẩn	Thiết lập môi trường build, biên dịch Haskell	✅
Client kết nối tới server qua TCP	Gửi và nhận dữ liệu dòng (line-based)	✅
Server nhận nhiều client (multi-thread)	Dùng forkIO để phục vụ nhiều client song song	⏳
Module Protocol	Chuẩn hóa định dạng gói tin (TV2 phụ trách)	⏳
Quản lý danh sách client	Lưu và broadcast tin nhắn (TV1 phụ trách)	⏳
Giao diện client thân thiện	Hiển thị rõ ràng tên người gửi, nội dung	⏳
Báo cáo cuối kỳ + README	Tài liệu hướng dẫn sử dụng & phân công	✅

🧩 Hướng phát triển tiếp theo
 Hoàn thiện Protocol.hs (định nghĩa cấu trúc tin nhắn)

 Bổ sung broadcast đến nhiều client

 Thêm xử lý tên người dùng

 Ghi log tin nhắn trên server

 Giao diện console đẹp hơn (màu sắc, thời gian gửi)

📜 License
Dự án phát hành dưới giấy phép MIT License
© 2025 — Phùng Chí Tâm & Phạm Duy Hoàng
