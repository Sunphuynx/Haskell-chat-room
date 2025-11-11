// static/auth.js
document.addEventListener("DOMContentLoaded", () => {
    const loginBtn = document.getElementById('login-btn');
    const regBtn = document.getElementById('reg-btn');
    const authError = document.getElementById('auth-error');

    function showError(message) {
        authError.textContent = message;
    }

    // Xu ly Dang Ky
    if (regBtn) {
        regBtn.onclick = async () => {
            const username = document.getElementById('reg-user').value;
            const password = document.getElementById('reg-pass').value;
            const confirm = document.getElementById('reg-pass-confirm').value;

            if (!username || !password || !confirm) {
                showError("Vui long nhap day du thong tin.");
                return;
            }
            if (password !== confirm) {
                showError("Mat khau xac nhan khong khop.");
                return;
            }

            try {
                const response = await fetch('/register', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ regUser: username, regPass: password, regPassConfirm: confirm })
                });

                if (response.ok) {
                    alert("Dang ky thanh cong! Vui long dang nhap.");
                    window.location.href = "/"; // Chuyen ve trang dang nhap
                } else {
                    const err = await response.json();
                    showError(err.message);
                }
            } catch (e) {
                showError("Khong the ket noi den server.");
            }
        };
    }

    // Xu ly Dang Nhap
    if (loginBtn) {
        loginBtn.onclick = async () => {
            const username = document.getElementById('login-user').value;
            const password = document.getElementById('login-pass').value;
            if (!username || !password) {
                showError("Vui long nhap ten dang nhap va mat khau.");
                return;
            }

            try {
                const response = await fetch('/login', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ loginUser: username, loginPass: password })
                });

                if (response.ok) {
                    const data = await response.json();
                    // Luu nickname vao localStorage de trang chat.html co a lay
                    localStorage.setItem('nickname', data.nickname);
                    window.location.href = "/chat"; // Chuyen den trang chat
                } else {
                    const err = await response.json();
                    showError(err.message);
                }
            } catch (e) {
                showError("Khong the ket noi den server.");
            }
        };
    }
});