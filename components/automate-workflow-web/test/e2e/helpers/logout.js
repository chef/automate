export default function logout() {
  browser.executeScript('localStorage.removeItem("Chefsession");');
}

global.logout = logout;
