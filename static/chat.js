// static/chat.js
document.addEventListener("DOMContentLoaded", () => {
    const nickname = localStorage.getItem('nickname');
    if (!nickname) {
        window.location.href = "/";
        return;
    }

    const fileInput = document.getElementById('file-input');
    const sendBtn = document.getElementById('send-btn');
    const messageInput = document.getElementById('message-input');
    const chatBox = document.getElementById('chat-box');
    const userListElement = document.getElementById('user-list');
    const userSearch = document.getElementById('user-search');
    const chatTargetElement = document.getElementById('chat-target');

    let socket = null;
    let allUsers = [];
    let chatTarget = "public";

    function connectWebSocket() {
        const wsProtocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsHost = window.location.host;
        socket = new WebSocket(`${wsProtocol}//${wsHost}/chat`);

        socket.onopen = () => {
            console.log("Đã kết nối WebSocket.");
            socket.send(JSON.stringify({ nickname: nickname }));
        };

        socket.onclose = () => {
            addMessageToBox(null, "Đã ngắt kết nối với server.", "info");
        };

        socket.onerror = (err) => {
            console.error("Lỗi WebSocket: ", err);
            addMessageToBox(null, "Lỗi kết nối.", "info");
        };

        socket.onmessage = (event) => {
            const msg = JSON.parse(event.data);
            
            if (msg.tag === 'Broadcast') {
                addMessageToBox(msg.contents[0], msg.contents[1], 'message');
            } else if (msg.tag === 'ReceivePrivateMessage') {
                addMessageToBox(msg.contents[0], `(riêng) ${msg.contents[1]}`, 'private');
            } else if (msg.tag === 'UserJoined') {
                addMessageToBox(null, `${msg.contents} đã tham gia.`, 'info');
            } else if (msg.tag === 'UserLeft') {
                addMessageToBox(null, `${msg.contents} đã rời.`, 'info');
            } else if (msg.tag === 'FileBroadcast') {
                addFileLinkToBox(msg.contents[0], msg.contents[1], msg.contents[2]);
            } else if (msg.tag === 'UserList') {
                updateUserList(msg.contents);
            } else if (msg.tag === 'LoadHistory') {
                chatBox.innerHTML = '';
                msg.contents[0].forEach(oldMsg => {
                    if (oldMsg.tag === 'Broadcast') {
                        addMessageToBox(oldMsg.contents[0], oldMsg.contents[1], 'message');
                    }
                });
            } else if (msg.tag === 'ServerInfo') {
                addMessageToBox(null, msg.contents, 'info');
            }
        };
    }

    fileInput.onchange = () => {
        const file = fileInput.files[0];
        if (file) {
            uploadFile(file);
        }
        fileInput.value = null;
    };

    async function uploadFile(file) {
        addMessageToBox(null, `Đang tải file ${file.name} lên...`, 'info');
        
        const formData = new FormData();
        formData.append('file', file);
        formData.append('nickname', nickname);

        try {
            const response = await fetch('/upload', {
                method: 'POST',
                body: formData
            });
            
            if (!response.ok) {
                const err = await response.json();
                addMessageToBox(null, `Lỗi: ${err.message}`, 'info');
            }
            
        } catch (e) {
            console.error("Lỗi upload: ", e);
            addMessageToBox(null, "Lỗi: Không thể kết nối để tải file.", "info");
        }
    }

    function updateUserList(users) {
        allUsers = users;
        userListElement.innerHTML = '';
        const createTargetEntry = (targetName, displayName) => {
            const li = document.createElement('li');
            li.textContent = displayName;
            if (chatTarget === targetName) {
                li.classList.add('active');
            }
            li.onclick = () => selectChatTarget(targetName, li);
            userListElement.appendChild(li);
        };
        createTargetEntry("public", "Tất Cả Mọi Người");
        users.forEach(user => {
            if (user === nickname) return;
            createTargetEntry(user, user);
        });
        filterUserList();
    }
    
    function selectChatTarget(target, element) {
        chatTarget = target;
        document.querySelectorAll('#user-list li').forEach(li => li.classList.remove('active'));
        element.classList.add('active');
        if (target === 'public') {
            chatTargetElement.innerHTML = "Đang chat với: <strong>Tất Cả Mọi Người</strong>";
        } else {
            chatTargetElement.innerHTML = `Đang chat riêng với: <strong>${target}</strong>`;
        }
    }
    
    const filterUserList = () => {
        const query = userSearch.value.toLowerCase();
        document.querySelectorAll('#user-list li').forEach(li => {
            const isPublic = li.textContent === "Tất Cả Mọi Người";
            if (isPublic) return;
            const match = li.textContent.toLowerCase().includes(query);
            li.style.display = match ? 'block' : 'none';
        });
    };
    userSearch.oninput = filterUserList;

    function sendMessage() {
        const content = messageInput.value;
        if (socket && content) {
            let msg = null;
            if (chatTarget === 'public') {
                msg = { tag: 'SendPublicMessage', contents: content };
            } else {
                msg = { tag: 'SendPrivateMessage', contents: [chatTarget, content] };
                addMessageToBox(nickname, `(riêng gửi ${chatTarget}) ${content}`, 'private-self');
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
        } else {
            msgDiv.textContent = content;
        }
        chatBox.appendChild(msgDiv);
        chatBox.scrollTop = chatBox.scrollHeight;
    }

    function addFileLinkToBox(sender, fileName, url) {
        const msgDiv = document.createElement('div');
        msgDiv.classList.add('message');

        const senderSpan = document.createElement('span');
        senderSpan.classList.add('sender');
        senderSpan.textContent = `${sender}: `;
        
        const contentSpan = document.createElement('span');
        contentSpan.classList.add('content');
        
        const fileLink = document.createElement('a');
        fileLink.href = url;
        fileLink.textContent = fileName;
        fileLink.target = "_blank";
        fileLink.classList.add('file-link');
        
        contentSpan.appendChild(document.createTextNode("Đã gửi một file: "));
        contentSpan.appendChild(fileLink);

        msgDiv.appendChild(senderSpan);
        msgDiv.appendChild(contentSpan);
        
        chatBox.appendChild(msgDiv);
        chatBox.scrollTop = chatBox.scrollHeight;
    }

    connectWebSocket();
});