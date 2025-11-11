// static/chat.js

document.addEventListener("DOMContentLoaded", () => {
    // Lay nickname tu localStorage
    const nickname = localStorage.getItem('nickname');
    if (!nickname) {
        // Neu khong co nickname (chua dang nhap), quay ve trang login
        window.location.href = "/";
        return;
    }

    // Lay cac phan tu giao dien
    const sendBtn = document.getElementById('send-btn');
    const messageInput = document.getElementById('message-input');
    const chatBox = document.getElementById('chat-box');
    const userListElement = document.getElementById('user-list');
    const userSearch = document.getElementById('user-search');
    const chatTargetElement = document.getElementById('chat-target');

    let socket = null;
    let allUsers = []; // Danh sach tat ca user
    let chatTarget = "public"; // Mac dinh la chat cong khai

    function connectWebSocket() {
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsHost = window.location.host;
        socket = new WebSocket(`${wsProtocol}//${wsHost}/chat`);

        socket.onopen = () => {
            console.log("Da ket noi WebSocket.");
            // Gui tin nhan xac thuc ngay khi ket noi
            socket.send(JSON.stringify({ nickname: nickname }));
        };

        socket.onclose = () => {
            addMessageToBox(null, "Da ngat ket noi voi server.", "info");
        };

        socket.onmessage = (event) => {
            const msg = JSON.parse(event.data);
            
            if (msg.tag === 'Broadcast') {
                addMessageToBox(msg.contents[0], msg.contents[1], 'message');
            } else if (msg.tag === 'ReceivePrivateMessage') {
                // Tu dong chon vao nguoi gui neu dang khong chat rieng
                if (chatTarget !== msg.contents[0]) {
                    // Kich hoat thong bao (vi du)
                }
                addMessageToBox(msg.contents[0], `(rieng) ${msg.contents[1]}`, 'private');
            } else if (msg.tag === 'UserJoined') {
                addMessageToBox(null, `${msg.contents[0]} da tham gia.`, 'info');
            } else if (msg.tag === 'UserLeft') {
                addMessageToBox(null, `${msg.contents[0]} da roi.`, 'info');
            } else if (msg.tag === 'UserList') {
                // *** SUA LOI 1 (NGHIEM TRONG) ***
                // Truyen vao ca danh sach 'msg.contents', khong phai 'msg.contents[0]'
                updateUserList(msg.contents);
            } else if (msg.tag === 'LoadHistory') {
                chatBox.innerHTML = '';
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

    // Cap nhat danh sach nguoi dung
    function updateUserList(users) {
        allUsers = users; // Luu lai de tim kiem
        userListElement.innerHTML = ''; // Xoa list cu
        
        // --- SUA LOI 2 (TRAI NGHIEM NGUOI DUNG) ---
        // Ham nay se tao mot muc trong danh sach va kiem tra xem
        // muc do co phai la muc dang duoc chon (chatTarget) hay khong.
        const createTargetEntry = (targetName, displayName) => {
            const li = document.createElement('li');
            li.textContent = displayName;
            if (chatTarget === targetName) {
                li.classList.add('active');
            }
            li.onclick = () => selectChatTarget(targetName, li);
            userListElement.appendChild(li);
        };
        // -------------------------------------------

        // Them nut chat "Tat Ca Moi Nguoi"
        createTargetEntry("public", "Tat Ca Moi Nguoi");

        // Them tung nguoi dung
        users.forEach(user => {
            if (user === nickname) return; // Khong hien thi ten minh
            createTargetEntry(user, user); // Ten muc tieu trung ten hien thi
        });
        
        // Loc lai danh sach theo thanh tim kiem (neu co)
        filterUserList();
    }

    // Chon muc tieu chat
    function selectChatTarget(target, element) {
        chatTarget = target;
        // Cap nhat giao dien
        document.querySelectorAll('#user-list li').forEach(li => li.classList.remove('active'));
        element.classList.add('active');
        if (target === 'public') {
            chatTargetElement.innerHTML = "Dang chat voi: <strong>Tat Ca Moi Nguoi</strong>";
        } else {
            chatTargetElement.innerHTML = `Dang chat rieng voi: <strong>${target}</strong>`;
        }
    }

    // Tim kiem nguoi dung
    const filterUserList = () => {
        const query = userSearch.value.toLowerCase();
        document.querySelectorAll('#user-list li').forEach(li => {
            const isPublic = li.textContent === "Tat Ca Moi Nguoi";
            if (isPublic) return; // Luon hien thi chat public
            
            const match = li.textContent.toLowerCase().includes(query);
            li.style.display = match ? 'block' : 'none';
        });
    };
    userSearch.oninput = filterUserList;


    // Gui tin nhan
    function sendMessage() {
        const content = messageInput.value;
        if (socket && content) {
            let msg = null;
            if (chatTarget === 'public') {
                msg = { tag: 'SendPublicMessage', contents: content };
                // Khong can hien thi tin nhan cua chinh minh o day
                // Server se broadcast lai cho tat ca, bao gom ca minh
            } else {
                msg = { tag: 'SendPrivateMessage', contents: [chatTarget, content] };
                // Hien thi ngay tin nhan rieng minh gui
                addMessageToBox(nickname, `(rieng gui ${chatTarget}) ${content}`, 'private-self');
            }
            
            socket.send(JSON.stringify(msg));
            messageInput.value = '';
        }
    }

    sendBtn.onclick = sendMessage;
    messageInput.onkeydown = (e) => {
        if (e.key === 'Enter') {
            e.preventDefault();
            sendMessage();
        }
    };

    // Ham them tin nhan vao giao dien
    function addMessageToBox(sender, content, type) {
        const msgDiv = document.createElement('div');
        msgDiv.classList.add(type); 

        if (type === 'message' || type === 'private' || type === 'private-self') {
            const senderSpan = document.createElement('span');
            senderSpan.classList.add('sender');
            senderSpan.textContent = `${sender}: `;
            
            const contentSpan = document.createElement('span');
            contentSpan.classList.add('content');
            contentSpan.textContent = content;

            msgDiv.appendChild(senderSpan);
            msgDiv.appendChild(contentSpan);
        } else { // 'info'
            msgDiv.textContent = content;
        }
        
        chatBox.appendChild(msgDiv);
        chatBox.scrollTop = chatBox.scrollHeight;
    }

    // Bat dau ket noi
    connectWebSocket();
});