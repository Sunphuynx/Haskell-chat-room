document.addEventListener("DOMContentLoaded", () => {
    const loginBtn = document.getElementById('login-btn');
    const regBtn = document.getElementById('reg-btn');
    const authError = document.getElementById('auth-error');

    function showError(message) {
        authError.textContent = message;
    }

    if (regBtn) {
        regBtn.onclick = async () => {
            const username = document.getElementById('reg-user').value;
            const password = document.getElementById('reg-pass').value;
            const confirm = document.getElementById('reg-pass-confirm').value;

            if (!username || !password || !confirm) {
                showError("Vui lòng nhập đầy đủ thông tin.");
                return;
            }
            if (password !== confirm) {
                showError("Mật khẩu xác nhận không khớp.");
                return;
            }

            try {
                const response = await fetch('/register', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ regUser: username, regPass: password, regPassConfirm: confirm })
                });

                if (response.ok) {
                    alert("Đăng ký thành công! Vui lòng chuyển sang trang đăng nhập.");
                    window.location.href = "/";
                } else {
                    const err = await response.json();
                    showError(err.message);
                }
            } catch (e) {
                showError("Không thể kết nối đến server.");
            }
        };
    }

    if (loginBtn) {
        loginBtn.onclick = async () => {
            const username = document.getElementById('login-user').value;
            const password = document.getElementById('login-pass').value;
            if (!username || !password) {
                showError("Vui lòng điền tên đăng nhập và mật khẩu.");
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
                    localStorage.setItem('nickname', data.nickname);
                    window.location.href = "/chat";
                } else {
                    const err = await response.json();
                    showError(err.message);
                }
            } catch (e) {
                showError("Không thể kết nối đến server.");
            }
        };
    }
});