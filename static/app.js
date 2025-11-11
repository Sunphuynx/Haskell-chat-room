// static/app.js

document.addEventListener("DOMContentLoaded", () => {
    // Lay cac phan tu giao dien
    const loginScreen = document.getElementById('login-screen');
    const chatScreen = document.getElementById('chat-screen');
    const loginBtn = document.getElementById('login-btn');
    const regBtn = document.getElementById('reg-btn');
    const sendBtn = document.getElementById('send-btn');
    const messageInput = document.getElementById('message-input');
    const chatBox = document.getElementById('chat-box');
    const authError = document.getElementById('auth-error');

    let socket = null;
    let nickname = null;

    // --- PHAN XU LY DANG KY ---
    regBtn.onclick = async () => {
        const username = document.getElementById('reg-user').value;
        const password = document.getElementById('reg-pass').value;
        if (!username || !password) {
            showError("Vui lòng nhập đầy đủ thông tin đăng ký.");
            return;
        }

        const response = await fetch('/register', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ authUser: username, authPass: password })
        });

        if (response.ok) {
            alert("Đăng ký thành công! Vui lòng chuyển sang trang đăng nhập.");
            showError(""); // Xoa loi
        } else {
            const err = await response.json();
            showError(err.message);
        }
    };

    // --- PHAN XU LY DANG NHAP ---
    loginBtn.onclick = async () => {
        const username = document.getElementById('login-user').value;
        const password = document.getElementById('login-pass').value;
        if (!username || !password) {
            showError("Vui lòng điền tên đăng nhập và mật khẩu.");
            return;
        }

        const response = await fetch('/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ authUser: username, authPass: password })
        });

        if (response.ok) {
            const data = await response.json();
            nickname = data.nickname;
            showChatScreen();
            connectWebSocket();
        } else {
            const err = await response.json();
            showError(err.message);
        }
    };

    function showError(message) {
        authError.textContent = message;
    }

    function showChatScreen() {
        loginScreen.style.display = 'none';
        chatScreen.classList.remove('hidden');
        chatScreen.style.display = 'flex'; // Hien thi man hinh chat
    }

    // --- PHAN XU LY CHAT (WEBSOCKET) ---
    function connectWebSocket() {
        // May chu web cua ban co the chay tren 'localhost:3000'
        // Khi dung ngrok, window.location.host se la dia chi ngrok
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsHost = window.location.host;
        socket = new WebSocket(`${wsProtocol}//${wsHost}/chat`);

        socket.onopen = () => {
            console.log("Đã kết nối WebSocket.");
            // TODO: Gui token hoac nickname de xac thuc
            // Vi du: socket.send(JSON.stringify({ type: "auth", token: "..." }));
        };

        socket.onclose = () => {
            addMessageToBox(null, "Đã ngắt kết nối với server.", "info");
        };

        socket.onerror = (err) => {
            console.error("Lỗi WebSocket: ", err);
            addMessageToBox(null, "Lỗi kết nối.", "info");
        };

        // Xu ly khi nhan duoc tin nhan tu server
        socket.onmessage = (event) => {
            const msg = JSON.parse(event.data);

            if (msg.tag === 'Broadcast') {
                addMessageToBox(msg.contents[0], msg.contents[1], 'message');
            } else if (msg.tag === 'UserJoined') {
                addMessageToBox(null, `${msg.contents[0]} đã tham gia phòng chat.`, 'info');
            } else if (msg.tag === 'UserLeft') {
                addMessageToBox(null, `${msg.contents[0]} đã rời phòng chat.`, 'info');
            } else if (msg.tag === 'LoadHistory') {
                msg.contents[0].forEach(oldMsg => {
                    if (oldMsg.tag === 'Broadcast') {
                        addMessageToBox(oldMsg.contents[0], oldMsg.contents[1], 'message');
                    }
                });
            } else if (msg.tag === 'ServerInfo') {
                addMessageToBox(null, msg.contents[0], 'info');
            }
        };
    }

    // Ham gui tin nhan
    function sendMessage() {
        const content = messageInput.value;
        if (socket && content) {
            const msg = {
                tag: 'SendPublicMessage',
                contents: content
            };
            socket.send(JSON.stringify(msg));
            messageInput.value = '';
        }
    }

    sendBtn.onclick = sendMessage;
    messageInput.onkeydown = (e) => {
        if (e.key === 'Enter') {
            sendMessage();
        }
    };

    // Ham them tin nhan vao giao dien
    function addMessageToBox(sender, content, type) {
        const msgDiv = document.createElement('div');
        msgDiv.classList.add(type); // 'message' hoac 'info'

        if (type === 'message') {
            const senderSpan = document.createElement('span');
            senderSpan.classList.add('sender');
            senderSpan.textContent = `${sender}: `;
            
            const contentSpan = document.createElement('span');
            contentSpan.classList.add('content');
            contentSpan.textContent = content;

            msgDiv.appendChild(senderSpan);
            msgDiv.appendChild(contentSpan);
        } else {
            msgDiv.textContent = content;
        }
        
        chatBox.appendChild(msgDiv);
        chatBox.scrollTop = chatBox.scrollHeight; // Tu dong cuon xuong duoi
    }
});