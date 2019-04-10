import LoginPage from '../page_objects/login.po';

export default function login() {

  let loginPage = new LoginPage();
  loginPage.get();

  let session = {
    enterprise: 'Chef',
    username: 'grrm',
    token: 'Qm+yP7LX1l/I1xW5pJg8NfEhgE1COkTeb3auVkRmQfc=',
    ttl: 12345
  };

  browser.executeScript(
    `localStorage.setItem('${session.enterprise}session',
    '${JSON.stringify(session)}');`
  );

  browser.executeScript(
    `localStorage.setItem('welcome-modal-seen', 'true');`
  );
}

global.login = login;
