const usernameInput = document.getElementById('username');
const passwordInput = document.getElementById('password');
const loginButton = document.getElementById('loginButton');

async function login() {
    // goes through the username and password input, then adds a event for enter ig
    const username = usernameInput.value;
    const password = passwordInput.value;

    if (!username || !password) {
        alert('Please enter both username and password.');
        return;
    }

    try {
        const res = await fetch('/api/login', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ user: username, password: password }) // <- must be 'user'
        });

        const data = await res.json();

        if (data.status === 'success') {
            // redirects
           window.location.href = 'main.html';
        } else {
            alert(data.message);
        }
    } catch (err) {
        console.error(err);
        alert('An error occurred while logging in.');
    }
}
// adds a click listener
loginButton.addEventListener('click', login);

// goes through the username and password input, then adds a event for enter ig
[usernameInput, passwordInput].forEach(input => {
    input.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            login();
        }
    });
});
