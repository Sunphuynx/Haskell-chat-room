# 💬 Ứng dụng Chat Room (Web App) bằng Haskell

Đây là dự án cuối kỳ cho môn học Lập Trình Hàm, được xây dựng hoàn toàn bằng Haskell. Dự án là một ứng dụng Web (Web Application) đầy đủ tính năng, sử dụng kiến trúc hiện đại với giao diện HTML/JS và backend Haskell.

Dự án này đáp ứng đầy đủ **7/7 yêu cầu** của đồ án:
1.  **Sử dụng Haskell:** Toàn bộ backend được viết bằng Haskell.
2.  **Chat / Truyền file:** Hỗ trợ chat (công khai, riêng tư) và upload/download file (.txt, .jpg, v.v.).
3.  **Client-Server:** Kiến trúc Web App với Client (Trình duyệt) và Server (Haskell).
4.  **Parallel:** Xử lý song song tác vụ đếm nguyên âm khi upload file `.txt`.
5.  **Concurrency:** Xử lý đồng thời nhiều kết nối HTTP và WebSocket.
6.  **Synchronization:** Sử dụng `STM` (TVar) để quản lý trạng thái server (danh sách user online) một cách an toàn.
7.  **Socket:** Sử dụng `WebSockets` cho giao tiếp hai chiều theo thời gian thực.

---

## 🧑‍💻 Thành viên nhóm

| Vai trò | Họ và tên | Nhiệm vụ |
|----------|------------|----------|
| **TV1** | **Phạm Duy Hoàng** | Phụ trách Backend (Server), logic Database, Xử lý song song, và Xác thực. |
| **TV2** | **Phùng Chí Tâm** | Phụ trách Frontend (HTML/CSS/JS), thiết kế Giao thức (Protocol), và tích hợp hệ thống. |

---

## ✨ Tính năng chính

* **Giao diện Web:** Giao diện người dùng hoàn chỉnh bằng HTML/CSS/JS, được chia thành các trang Đăng nhập, Đăng ký, và Phòng chat.
* **Xác thực Người dùng:**
    * Đăng ký tài khoản (tên đăng nhập + mật khẩu).
    * Xác nhận mật khẩu khi đăng ký.
    * Đăng nhập bằng tài khoản đã tạo.
* **Bảo mật:** Mật khẩu người dùng được "băm" (hash) an toàn bằng thuật toán **PBKDF2 (SHA256)** trước khi lưu vào database.
* **Lưu trữ Dữ liệu:**
    * Sử dụng **SQLite** (`chat.db`) để lưu trữ vĩnh viễn thông tin người dùng và lịch sử tin nhắn.
    * Tự động tải lịch sử chat cũ khi người dùng vào phòng.
* **Chat Real-time:**
    * Sử dụng **WebSockets** để gửi và nhận tin nhắn ngay lập tức.
    * **Chat Công khai:** Gửi tin nhắn cho tất cả mọi người.
    * **Chat Riêng tư:** Nhấn vào tên người dùng trong danh sách để gửi tin nhắn riêng.
    * **Danh sách Online:** Tự động cập nhật danh sách người dùng đang online (và có thể tìm kiếm).
* **Truyền File:**
    * Hỗ trợ upload **mọi loại file** (.txt, .jpg, .png, .pdf...) qua HTTP.
    * File upload được lưu trong thư mục `uploads/` trên server.
    * Server tự động gửi link tải file vào phòng chat cho mọi người.
* **Xử lý Song song (Parallel):**
    * Khi người dùng upload file `.txt`, server sẽ tự động chạy một tác vụ **song song** (parallel) để đếm số lượng nguyên âm trong file đó và thông báo kết quả ra phòng chat.

## 🛠️ Công nghệ sử dụng

| Lĩnh vực | Công nghệ | Mục đích |
| :--- | :--- | :--- |
| **Backend** | **Haskell** (GHC 9.6.5) | Ngôn ngữ chính để xây dựng toàn bộ logic server. |
| | **Stack** | Quản lý build và các thư viện phụ thuộc. |
| | **Scotty** & **Warp** | Dùng để tạo Web Server (HTTP), xử lý các route API (login, register, upload). |
| | **WebSockets** | Xử lý giao tiếp hai chiều theo thời gian thực (chat). |
| | **SQLite-Simple** | Tương tác với database `chat.db` (lưu user, messages). |
| | **STM (TVar)** | Đồng bộ hóa (Synchronization) trạng thái server (danh sách user online). |
| | **Cryptonite** | Băm và xác thực mật khẩu người dùng một cách an toàn. |
| | **Parallel** | Cung cấp các hàm xử lý song song (`parList`, `rseq`). |
| **Frontend** | **HTML5** | Xây dựng cấu trúc 3 trang: `login.html`, `register.html`, `chat.html`. |
| | **CSS3** | Tạo kiểu và làm đẹp cho giao diện người dùng. |
| | **JavaScript (ES6+)** | Xử lý toàn bộ logic phía client (Fetch API, DOM, WebSocket). |
| **Giao tiếp** | **JSON (Aeson)** | Định dạng dữ liệu chính để giao tiếp giữa JavaScript và server Haskell. |

## 🚀 Hướng dẫn Cài đặt & Chạy
**Yêu cầu:** Cần cài đặt [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (sẽ tự động cài GHC).

**1. Clone dự án:**
```
git clone [https://github.com/Sunphuynx/Haskell-chat-room.git]

(https://github.com/Sunphuynx/Haskell-chat-room.git)

cd haskell-chat-room
```
**2. Build dự án:** Lần đầu tiên build sẽ mất một lúc để tải và cài đặt các thư viện (Scotty, WebSockets, SQLite...).
```
stack build
```
**3. Chạy Server:**

```
stack exec server-exe
```

Bạn sẽ thấy thông báo:
```
Khoi dong server tren port 3000...
Dang chay server tren port 3000...
```
**4. Mở ứng dụng:** Mở trình duyệt (Chrome, Firefox,...) và truy cập vào địa chỉ:

```
http://localhost:3000
```
Bạn sẽ thấy trang đăng nhập. Hãy tạo một vài tài khoản và mở nhiều tab trình duyệt để bắt đầu chat!

## 🌐 Demo qua Internet (với ngrok)

Để cho phép bạn bè từ bên ngoài mạng của bạn truy cập vào ứng dụng, bạn có thể sử dụng ngrok.

1. Đảm bảo server của bạn đang chạy (stack exec server-exe).

2. Mở một terminal khác và chạy:

```
ngrok http 3000
```
3. ngrok sẽ cho bạn một đường link https://... công khai.
   
4. Gửi đường link https://... đó cho bạn bè. Họ có thể truy cập, đăng ký và chat với bạn qua trình duyệt của họ.

## ☁️ Demo Trực tuyến (Triển khai trên Google Cloud)

Phiên bản mới nhất của ứng dụng này đã được triển khai (deploy) thành công lên **Google Cloud Platform (GCP)** bằng dịch vụ Compute Engine.

Bạn có thể truy cập và sử dụng ứng dụng chat ngay lập tức mà không cần cài đặt bất cứ thứ gì thông qua đường dẫn dưới đây:

**➡️ Link Truy cập: [http://35.197.156.188:3000](http://35.197.156.188:3000)**

#### Hướng dẫn truy cập:

1.  Mở trình duyệt (Chrome, Firefox,...) của bạn.
2.  Truy cập vào địa chỉ: `http://35.197.156.188:3000`
3.  Bạn sẽ thấy trang đăng nhập. Hãy tạo một tài khoản mới để bắt đầu trải nghiệm.

*Lưu ý: Đây là một máy chủ demo phục vụ cho đồ án, máy chủ có thể được khởi động lại bất cứ lúc nào và dữ liệu (tin nhắn, tài khoản) có thể bị xóa định kỳ. Địa chỉ IP này là địa chỉ IP tạm thời (Ephemeral) của máy ảo.*
