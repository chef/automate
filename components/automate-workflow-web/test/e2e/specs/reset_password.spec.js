import ResetPasswordPage from '../page_objects/reset_password.po';

describe('reset password page', () => {

  let resetPasswordPage;

  beforeEach(() => {
    resetPasswordPage = new ResetPasswordPage();
    resetPasswordPage.get('alice', 'to%252Fken');
  });

  afterEach(() => {
    mockApi.clear();
  });

  describe('initial state', () => {
    describe('the submit button', () => {
      it('is disabled', () => {
        expect(resetPasswordPage.submitButton.isEnabled()).toBe(false);
      });
    });
  });

  describe('when new password entry matches "repeat new password" entry', () => {
    beforeEach(() => {
      resetPasswordPage.enter('foo', 'foo');
    });

    describe('the submit button', () => {
      it('is enabled', () => {
        expect(resetPasswordPage.submitButton.isEnabled()).toBe(true);
      });
    });

    describe('clicking submit', () => {

      describe('and the request succeeds', () => {
        beforeEach(() => {
          mockApi([
              {
                request: {
                  url: '/api/v0/e/Chef/internal-users/alice/reset-password$',
                  method: 'POST',
                  body: {
                    token: 'to/ken',
                    password: 'foo'
                  }
                },
                response: {
                  status: 204
                }
              }]);
          resetPasswordPage.submitButton.click();
        });

        it('redirects to login page', () => {
          expect(browser.getCurrentUrl()).toContain('login');
        });
      });

      describe('and the request fails because the new password is under 4 characters', () => {
        beforeEach(() => {
          mockApi([
              {
                request: {
                  url: '/api/v0/e/Chef/internal-users/alice/reset-password$',
                  method: 'POST',
                  body: {
                    token: 'to/ken',
                    password: 'foo'
                  }
                },
                response: {
                  status: 400,
                  body: {
                    error: 'invalid_password',
                    message: 'Password length must be greater than 3 characters.'
                  }
                }
              }]);
          resetPasswordPage.submitButton.click();
        });

        it('displays the error', () => {
          expect(resetPasswordPage.errorMessageText.getText()).toBe(
              'Password length must be greater than 3 characters. - Your password reset link expired.' +
              ' Contact your Chef Automate admin to request a new reset link.'
              );
        });

        it('disables the submit button', () => {
          expect(resetPasswordPage.submitButton.isEnabled()).toBe(false);
        });

        it('hides the password inputs', () => {
          expect(resetPasswordPage.passwordInput).not.toBePresent();
          expect(resetPasswordPage.repeatedPasswordInput).not.toBePresent();
        });
      });

      describe('and the request fails because the token was not found for this user', () => {
        beforeEach(() => {
          mockApi([
              {
                request: {
                  url: '/api/v0/e/Chef/internal-users/alice/reset-password$',
                  method: 'POST',
                  body: {
                    token: 'to/ken',
                    password: 'foo'
                  }
                },
                response: {
                  status: 401,
                  body: {
                    error: 'not_authorized'
                  }
                }
              }]);
          resetPasswordPage.submitButton.click();
        });

        it('displays the error', () => {
          expect(resetPasswordPage.errorMessageText.getText()).toBe(
            '401 - Unauthorized - Your password reset link expired.' +
            ' Contact your Chef Automate admin to request a new reset link.'
            );
        });

        it('disables the submit button', () => {
          expect(resetPasswordPage.submitButton.isEnabled()).toBe(false);
        });

        it('hides the password inputs', () => {
          expect(resetPasswordPage.passwordInput).not.toBePresent();
          expect(resetPasswordPage.repeatedPasswordInput).not.toBePresent();
        });
      });

      describe('and the request fails because of an internal server error', () => {
        beforeEach(() => {
          mockApi([
              {
                request: {
                  url: '/api/v0/e/Chef/internal-users/alice/reset-password$',
                  method: 'POST',
                  body: {
                    token: 'to/ken',
                    password: 'foo'
                  }
                },
                response: {
                  status: 500,
                  body: {
                    error: 'internal_server_error'
                  }
                }
              }]);
          resetPasswordPage.submitButton.click();
        });

        it('displays the error', () => {
          expect(resetPasswordPage.errorMessageText.getText()).toBe('500 - Internal Server Error');
        });

        it('does not disable the submit button', () => {
          expect(resetPasswordPage.submitButton.isEnabled()).toBe(true);
        });

        it('does not hide the password inputs', () => {
          expect(resetPasswordPage.passwordInput).toBePresent();
          expect(resetPasswordPage.repeatedPasswordInput).toBePresent();
        });
      });
    });
  });

  describe('when new password entry does not match "repeat new password" entry', () => {
    beforeEach(() => {
      resetPasswordPage.enter('foo', 'bar');
    });

    describe('the submit button', () => {
      it('is disabled', () => {
        expect(resetPasswordPage.submitButton.isEnabled()).toBe(false);
      });
    });
  });
});
