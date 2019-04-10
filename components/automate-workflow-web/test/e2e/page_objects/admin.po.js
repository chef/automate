export default class AdminPage {
  get() {
    browser.get('#/users');
  }

  get userTab() {
    return element(by.linkText('Users'));
  }

  get scmSetupTab() {
    return element(by.linkText('SCM Setup'));
  }

  get emailSetupTab() {
    return element(by.linkText('Email Setup'));
  }
}
