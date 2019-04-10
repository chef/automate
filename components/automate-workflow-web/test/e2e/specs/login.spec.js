import LoginPage from '../page_objects/login.po';
import unauthorizedLogin from '../mocks/unauthorized_login.mock';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('login page', () => {

  let loginPage;

  beforeAll(() => {
    loginPage = new LoginPage();
  });

  beforeEach(() => {
    // Ensure that we start on the login page.
    // There are tests in this module that cause redirects
    // in the browser to test the SAML functionality, and
    // if we're not on the login page when the tests start,
    // then we may be trying to do things like resetting
    // the feature flags from a page that's unexpected
    loginPage.get();
  });

  afterEach(() => {
    mockApi.clear();
  });

  describe('with SAML session information in cookie', () => {

    beforeEach(() => {
      mockApi([authorizedLogin]);

      loginPage.get(
        { returnUrl: '#/dashboard' },
        {
          'saml-chef-delivery-token': 'VE9LRU4h',
          'saml-chef-delivery-user': 'kimmy',
          'saml-chef-delivery-ttl': '3600'
        }
      );
    });

    // Session.set() will write to localStorage
    afterAll(() => {
      browser.executeScript('window.localStorage.clear();');
    });

    it('should set the session user and token', () => {
      var session = browser.executeScript('return window.localStorage.getItem("Chefsession");');
      expect(session).toBe('{"enterprise":"Chef","token":"VE9LRU4h","username":"kimmy","ttl":"3600"}');
    });

    it('should remove the saml information cookies', () => {
      browser.manage().getCookie("saml-chef-delivery-token").then( (cookie) => {
          expect(cookie).toBe(null);
      });
      browser.manage().getCookie("saml-chef-delivery-user").then( (cookie) => {
          expect(cookie).toBe(null);
      });
    });

    it('should create the session information cookies', () => {
      browser.manage().getCookie("chef-delivery-token").then( (cookie) => {
          expect(cookie.value).toBe("VE9LRU4h");
      });
      browser.manage().getCookie("chef-delivery-user").then( (cookie) => {
          expect(cookie.value).toBe("kimmy");
      });
    });

    it('should forward you to the dashboard', () => {
      browser.wait(urlContains('/e/Chef/#/dashboard'), 5000);
      expect(browser.driver.getCurrentUrl()).toContain('/e/Chef/#/dashboard');
    });
  });

  describe('with an error message as a query param', () => {

    let errorMessage = 'this is an error message';
    let errorBase64 = 'dGhpcyBpcyBhbiBlcnJvciBtZXNzYWdl'; // btoa(errorMessage)

    beforeEach(() => {
      loginPage.get({error: errorBase64});
    });

    it('should display the error message', () => {
      expect(loginPage.messageText).toBeDisplayed();
      expect(loginPage.messageText.getText()).toEqual(errorMessage);
      expect(loginPage.messageText).toHaveClass('error');
    });
  });

  describe('with saml enabled', () => {

    beforeEach(() => {
      mockApi([
        {
          request: {
            url: '/api/v0/e/Chef/saml/enabled$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              enabled: true
            }
          }
        }
      ]);
      loginPage.get();
    });

    it('should display a "Sign In" button with no password field', () => {
      expect(loginPage.passwordInput).not.toBePresent();
      expect(loginPage.submitButton).toBeDisplayed();
    });

    it('should a link to sign in with a password', () => {
      expect(element(by.linkText('Sign in with username and password'))).toBeDisplayed();
    });

    it('should set the class of the login container to "saml"', () => {
      expect(loginPage.loginContainer).toHaveClass('saml');
    });

    describe('when pressing the "Sign In" button', () => {
      beforeEach(() => {
        mockApi([
          {
            request: {
              url: '/api/v0/e/Chef/saml/auth',
              method: 'GET'
            },
            response: {
              status: 302,
              headers: {
                'Location': '/fakeSamlLoginUrl'
              }
            }
          },
          {
            request: {
              url: '/fakeSamlLoginUrl',
              method: 'GET'
            },
            response: {
              status: 200,
              headers: {
                'Content-type': 'text/html'
              },
              body: `
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Fake SAML Login</title>
  </head>
  <body>
    Here is a body
  </body>
</html>`
            }
          }
        ]);
      });

      it('should redirect to the IdP', () => {
        loginPage.saml();
        browser.driver.wait(function() {
          return browser.driver.getCurrentUrl().then(function(url) {
            return /fake/.test(url);
          });
        }, 10000);
        expect(browser.driver.getTitle()).toEqual('Fake SAML Login');
      });
    });

    describe('when clicking through to the local login page', () => {

      beforeEach(() => {
        loginPage.toggleLocal();
      });

      it('should display a username and password field', () => {
        expect(loginPage.userNameInput).toBePresent();
        expect(loginPage.passwordInput).toBePresent();
      });

      it('should display a link to authenticate with SAML', () => {
        expect(loginPage.samlLink).toBePresent();
      });

      it('should set the class of the login container to "local"', () => {
        expect(loginPage.loginContainer).toHaveClass('local');
      });
    });
  });

  describe('with saml disabled', () => {

    beforeEach(() => {
      mockApi([
        {
          request: {
            url: '/api/v0/e/Chef/saml/enabled',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              enabled: false
            }
          }
        }
      ]);
      loginPage.get({ returnUrl: '#/dashboard' });
    });

    it('should display a username and password field', () => {
      expect(loginPage.userNameInput).toBePresent();
      expect(loginPage.passwordInput).toBePresent();
    });

    it('should not display a link to authenticate with SAML', () => {
      expect(loginPage.samlLink).not.toBePresent();
    });

    it('should set the class of the login container to "local"', () => {
      expect(loginPage.loginContainer).toHaveClass('local');
    });

    describe('logging in', () => {

      describe('with invalid credentials', () => {

        beforeEach(() => {
          mockApi([
            unauthorizedLogin
          ]);

          loginPage.login('grrm', 'wrongpassword');
        });

        it('should not log you in', () => {
          expect(browser.getCurrentUrl()).toContain('login');
        });

        it('should display an error message', () => {
          expect(loginPage.messageText).toBeDisplayed();
          expect(loginPage.messageText).toHaveClass('error');
        });
      });

      describe('with valid credentials', () => {

        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/get-token$',
                method: 'POST'
              },
              response: {
                status: 201,
                body: {
                  token: 'Qm+yP7LX1l/I1xW5pJg8NfEhgE1COkTeb3auVkRmQfc='
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/orgs$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  orgs: []
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/users/grrm$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  name: 'grrm',
                  first: 'George',
                  last: 'Marten',
                  email: 'george@getchef.com',
                  ssh_pub_key: 'cool_pub_key'
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/authz/users/grrm$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  admin: ['enterprise'],
                  committer: ['enterprise'],
                  reviewer: ['enterprise'],
                  shipper: ['enterprise'],
                  observer: ['enterprise']
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/users$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  users: [
                    'a',
                    'b',
                    'c',
                    'd'
                  ]
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/pipelines$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: [
                  {
                    id: 1,
                    org: 'fooOrg',
                    project: 'fooProject'
                  },
                  {
                    id: 2,
                    org: 'barOrg',
                    project: 'barProject'
                  },
                  {
                    id: 3,
                    org: 'bazOrg',
                    project: 'bazProject'
                  }
                ]
              }
            }
          ]);

          loginPage.login('grrm', 'grrm');
        });

        afterEach(logout);

        it('should log you in', () => {
          browser.wait(urlContains('/e/Chef/#/dashboard'), 5000);
          expect(browser.driver.getCurrentUrl()).toContain('/e/Chef/#/dashboard');
        });
      });

      describe('with trouble reaching the backend', () => {

        beforeEach(() => {
          mockApi([
            {
              request: {
                url: '/api/v0/e/Chef/verify-token$', // first url that is hit
                method: 'HEAD'
              },
              response: {
                status: 502 // Bad Gateway (delivery-web down)
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/get-token$', // this will be tried next
                method: 'POST'
              },
              response: {
                status: 502
              }
            },
          ]);

          loginPage.login('grrm', 'correctpassword');
        });

        it('should not log you in', () => {
          expect(browser.getCurrentUrl()).toContain('login');
        });

        it('should display an error message', () => {
          expect(loginPage.messageText).toBeDisplayed();
          expect(loginPage.messageText).toHaveClass('error');
        });
      });
    });
  });
});
