/*
 * Note that for ng2, some matchers are not available:
 *   - by.repeater
 *   - by.model
 *   - by.binding
 * See https://angular.io/docs/ts/latest/guide/upgrade.html#!#appendix-upgrading-phonecat-tests
 */
export default class ResetPasswordPage {

  get(username, token) {
    browser.get(`#/reset-password/${username}/${token}`);
  }

  get passwordInput() {
    return element(by.name('password1'));
  }

  get repeatedPasswordInput() {
    return element(by.name('password2'));
  }

  get submitButton() {
    return element(by.buttonText('Set New Password'));
  }

  get errorMessageText() {
    return element(by.css('.error-message'));
  }

  get successMessageText() {
    return element(by.css('.success-message'));
  }

  get gotoLoginButton() {
    return element(by.buttonText('Go to Login'));
  }

  enter(password1, password2) {
    this.passwordInput.sendKeys(password1);
    this.repeatedPasswordInput.sendKeys(password2);
  }
}
