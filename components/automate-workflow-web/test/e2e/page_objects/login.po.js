export default class LoginPage {

  get(params, cookies) {
    var loginString = '#/login';
    var query = [];
    for (var p in params) {
      query.push(encodeURIComponent(p) + "=" + encodeURIComponent(params[p]));
    }
    for (var c in cookies) {
      browser.manage().addCookie(c, cookies[c]);
    }
    browser.get(loginString + '?' + query.join('&'));
  }

  get passwordInput() {
    return element(by.model('user.password'));
  }

  get submitButton() {
    return element(by.buttonText('Sign In'));
  }

  get messageText() {
    return element(by.binding('message'));
  }

  get userNameInput() {
    return element(by.model('user.username'));
  }

  get samlLink() {
    return element(by.id('link-toggle-saml'));
  }

  get localLink() {
    return element(by.id('link-toggle-local'));
  }

  get loginContainer() {
    return element(by.css('.login-container'));
  }

  login(username, password) {
    this.userNameInput.sendKeys(username);
    this.passwordInput.sendKeys(password);
    this.submitButton.click();
  }

  saml() {
    this.submitButton.click();
  }

  toggleLocal() {
    this.localLink.click();
  }

  toggleSaml() {
    this.samlLink.click();
  }
}
