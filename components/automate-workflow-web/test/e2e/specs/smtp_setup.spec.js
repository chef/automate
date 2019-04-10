import SmtpSetupPage from '../page_objects/smtp_setup.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('smtp setup page', () => {
  let smtpSetupPage, mocks;

  beforeAll(login);
  afterAll(logout);

  beforeEach(() => {
    mockApi.clear();

    mocks = [
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/orgs$',
          method: 'GET'
        },
        response: {
          body: {
            _links: {},
            orgs: []
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/users$',
          method: 'GET'
        },
        response: {
          body: {
            _links: {},
            users: []
          }
        }
      }
    ];

    smtpSetupPage = new SmtpSetupPage();
  });

  describe('when the user is not an enterprise admin', () => {

    beforeEach(() => {
      mocks.push({
        request: {
          url: '/api/v0/e/Chef/authz/users/grrm$',
          method: 'GET'
        },
        response: {
          status: 403
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/users/grrm$',
          method: 'GET'
        },
        response: {
          status: 200
        }
      });

      mockApi(mocks);
      smtpSetupPage.get();
    });

    it('shows the forbidden message', () => {
      expect(smtpSetupPage.forbiddenText.isPresent()).toBe(true);
    });
  });

  describe('when the user is an enterprise admin', () => {

    beforeEach(() => {
      mocks.push({
        request: {
          url: '/api/v0/e/Chef/authz/users/grrm$',
          method: 'GET'
        },
        response: {
          status: 200
        }
      });
    });

    describe('and the user has no email address', () => {

      beforeEach(() => {

        mocks.push({
          request: {
            url: '/api/v0/e/Chef/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              email: null
            }
          }
        });

        mockApi(mocks);
        smtpSetupPage.get();
      });

      it('disables the test button', () => {
        expect(smtpSetupPage.testConnectionButton.isEnabled()).toBe(false);
      });

      it('labels the test button without an email address', () => {
        expect(smtpSetupPage.testConnectionButton.getText()).toBe('SEND TEST');
      });
    });

    describe('and the user has an email address', () => {

      beforeEach(() => {
        mocks.push({
          request: {
            url: '/api/v0/e/Chef/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              email: 'grrm@westeros.net'
            }
          }
        });

        mockApi(mocks);
        smtpSetupPage.get();
      });

      it('disables the test button when required fields are missing', () => {
        expect(smtpSetupPage.testConnectionButton.isEnabled()).toBe(false);
      });

      describe('and login and password fields are filled in', () => {

        beforeEach(() => {
          smtpSetupPage.smtpHost.sendKeys('smtp.somewhere.com');
          smtpSetupPage.smtpPort.sendKeys('25');
          smtpSetupPage.smtpLogin.sendKeys('smtp@google.com');
          smtpSetupPage.smtpPassword.sendKeys('ithappeningonenight');
          smtpSetupPage.smtpSenderEmail.sendKeys('helper@ventech.co');
          smtpSetupPage.smtpSenderName.sendKeys('helper');
        });

        it('enables the test button', () => {
          expect(smtpSetupPage.testConnectionButton.isEnabled()).toBe(true);
        });

        describe('and the test button is clicked', () => {

          describe('and the test is successful', () => {

            beforeEach(() => {
              mocks.push({
                request: {
                  url: `/api/v0/e/Chef/notifications/smtp/test$`,
                  method: 'POST'
                },
                response: {
                  status: 200
                }
              });

              mockApi(mocks);
              smtpSetupPage.testConnectionButton.click();
            });

            it('shows a success message', () => {
              expect(smtpSetupPage.smtpTestSuccessMessage).toBePresent();
            });
          });

          describe('and the email config is invalid', () => {

            beforeEach(() => {
              mocks.push({
                request: {
                  url: `/api/v0/e/Chef/notifications/smtp/test$`,
                  method: 'POST'
                },
                response: {
                  status: 404
                }
              });

              mockApi(mocks);
              smtpSetupPage.testConnectionButton.click();
            });

            it('shows a failure message', () => {
              expect(smtpSetupPage.smtpTestUnsuccessfulMessage).toBePresent();
            });
          });
        });

        describe('and the config is saved successfully', () => {
          // This is needed to check if the flash message is displayed
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          beforeEach(() => {
            // TODO: the body isn't actually enforced?
            mocks.push({
              request: {
                url: `/api/v0/e/Chef/notifications/smtp$`,
                method: 'PUT',
                body: {
                  host: 'smtp.somewhere.com',
                  smtp_login: 'smtp@google.com',
                  password: 'ithappeningonenight',
                  port: 25,
                  sender_email: 'helper@ventech.co',
                  sender_name: 'helper'
                }
              },
              response: {
                status: 201
              }
            });

            mockApi(mocks);
            smtpSetupPage.saveButton.click();
          });

          // Marked pending because of occasional failures
          xit('opens a success Flash modal', () =>{
            expect(smtpSetupPage.configSaveSuccessFlash).toBeDisplayed();
          });
        });
      });

      describe('and only the required fields are filled in', () => {
        // See describe block above (with password and login)
        beforeEach(() => {
          browser.ignoreSynchronization = true;
        });

        afterEach(() => {
          browser.ignoreSynchronization = false;
        });

        beforeEach(() => {
          smtpSetupPage.smtpHost.sendKeys('smtp.somewhere.com');
          smtpSetupPage.smtpPort.sendKeys('25');
          smtpSetupPage.smtpSenderEmail.sendKeys('helper@ventech.co');
          smtpSetupPage.smtpSenderName.sendKeys('helper');
        });

        it('enables the test button', () => {
          expect(smtpSetupPage.testConnectionButton.isEnabled()).toBe(true);
        });

        describe('and the test button is clicked', () => {

          describe('and the test is successful', () => {

            beforeEach(() => {
              mocks.push({
                request: {
                  url: `/api/v0/e/Chef/notifications/smtp/test$`,
                  method: 'POST'
                },
                response: {
                  status: 200
                }
              });

              mockApi(mocks);
              smtpSetupPage.testConnectionButton.click();
            });

            it('shows a success message', () => {
              expect(smtpSetupPage.smtpTestSuccessMessage).toBePresent();
            });
          });

          describe('and the email config is invalid', () => {

            beforeEach(() => {
              mocks.push({
                request: {
                  url: `/api/v0/e/Chef/notifications/smtp/test$`,
                  method: 'POST'
                },
                response: {
                  status: 404
                }
              });

              mockApi(mocks);
              smtpSetupPage.testConnectionButton.click();
            });

            it('shows a failure message', () => {
              expect(smtpSetupPage.smtpTestUnsuccessfulMessage).toBePresent();
            });
          });
        });

        describe('and the config is saved successfully', () => {

          beforeEach(() => {
            // TODO: the body isn't actually enforced?
            mocks.push({
              request: {
                url: `/api/v0/e/Chef/notifications/smtp$`,
                method: 'PUT',
                body: {
                  host: 'smtp2.somewhere.com',
                  port: 25,
                  sender_email: 'helper@ventech.co',
                  sender_name: 'helper'
                }
              },
              response: {
                status: 201
              }
            });

            mockApi(mocks);
            smtpSetupPage.saveButton.click();
          });

          // Marked pending because of occasional failures
          xit('opens a success Flash modal', () =>{
            expect(smtpSetupPage.configSaveSuccessFlash).toBeDisplayed();
          });
        });
      });
    });

    describe('and email has not been not set up', () =>{

      beforeEach(() => {
        mocks.push({
          request: {
            url: '/api/v0/e/Chef/notifications/smtp',
            method: 'GET'
          },
          response: {
            status: 404
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200
          }
        });

        mockApi(mocks);
        smtpSetupPage.get();
      });

      it('shows the setup form', () => {
        expect(smtpSetupPage.smtpSetupFields.isPresent()).toBe(true);
      });

      it('shows the default settings', () => {
        expect(smtpSetupPage.smtpHost.getAttribute('value')).toEqual('');
        expect(smtpSetupPage.smtpPort.getAttribute('value')).toEqual('');
        expect(smtpSetupPage.smtpLogin.getAttribute('value')).toEqual('');
        expect(smtpSetupPage.smtpPassword.getAttribute('value')).toEqual('');
        expect(smtpSetupPage.smtpSenderEmail.getAttribute('value')).toEqual('');
        expect(smtpSetupPage.smtpSenderName.getAttribute('value')).toEqual('');
      });

      it('disables the test button', () => {
        expect(smtpSetupPage.testConnectionButton.isEnabled()).toBe(false);
      });

      it('does not display the remove button', () => {
        expect(smtpSetupPage.removeButton.isPresent()).toBe(false);
      });
    });

    describe('and email has been set up', () => {

      beforeEach(() => {
        mocks.push(
          {
            request: {
              url: '/api/v0/e/Chef/users/grrm$',
              method: 'GET'
            },
            response: {
              status: 200
            }
          }
        );
      });

      describe('the displayed config', () => {

        describe('when optional fields are not provided', () => {

          beforeEach(() => {
            mocks.push(
              {
                request: {
                  url: '/api/v0/e/Chef/notifications/smtp',
                  method: 'GET'
                },
                response: {
                  status: 200,
                  body: {
                    host: "beep.boop",
                    port: 25,
                    smtp_login: "roboty",
                    sender_email: "name@bomb.com"
                  }
                }
              }
            );

            mockApi(mocks);
            smtpSetupPage.get();
          });

          it('displays the config without optional values', () => {
            expect(smtpSetupPage.smtpHost.getAttribute('value')).toEqual('beep.boop');
            expect(smtpSetupPage.smtpPort.getAttribute('value')).toEqual('25');
            expect(smtpSetupPage.smtpLogin.getAttribute('value')).toEqual('roboty');
            expect(smtpSetupPage.smtpSenderEmail.getAttribute('value')).toEqual('name@bomb.com');
            expect(smtpSetupPage.smtpSenderName.getAttribute('value')).toEqual('');
          });

          it('displays dummy password', () => {
            expect(smtpSetupPage.smtpPassword.getAttribute('value')).toEqual('**********');
          });

          it('displays the remove button', () => {
            expect(smtpSetupPage.removeButton.isPresent()).toBe(true);
          });
        });

        describe('when optional fields are provided', () => {

          beforeEach(() => {
            mocks.push(
              {
                request: {
                  url: '/api/v0/e/Chef/notifications/smtp',
                  method: 'GET'
                },
                response: {
                  status: 200,
                  body: {
                    host: "beep.boop",
                    port: 25,
                    smtp_login: "roboty",
                    sender_email: "name@bomb.com",
                    sender_name: "namez"
                  }
                }
              }
            );

            mockApi(mocks);
            smtpSetupPage.get();
          });

          it('displays the config', () => {
            expect(smtpSetupPage.smtpHost.getAttribute('value')).toEqual('beep.boop');
            expect(smtpSetupPage.smtpPort.getAttribute('value')).toEqual('25');
            expect(smtpSetupPage.smtpLogin.getAttribute('value')).toEqual('roboty');
            expect(smtpSetupPage.smtpSenderEmail.getAttribute('value')).toEqual('name@bomb.com');
            expect(smtpSetupPage.smtpSenderName.getAttribute('value')).toEqual('namez');
          });

          it('displays the dummy password', () => {
            expect(smtpSetupPage.smtpPassword.getAttribute('value')).toEqual('**********');
          });

          it('displays the remove button', () => {
            expect(smtpSetupPage.removeButton.isPresent()).toBe(true);
          });
        });
      });

      describe('and the remove button is clicked', () => {
        beforeEach(() => {
          mocks.push(
            {
              request: {
                url: '/api/v0/e/Chef/notifications/smtp',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  host: "beep.boop",
                  port: 25,
                  smtp_login: "roboty",
                  sender_email: "name@bomb.com",
                  sender_name: "namez"
                }
              }
            }
          );

          mockApi(mocks);
          smtpSetupPage.get();
        });

        it('opens a Flash modal', () =>{
          expect(smtpSetupPage.removeSmtpConfirmationModal.isPresent()).toBe(false);
          smtpSetupPage.removeButton.click();
          expect(smtpSetupPage.removeSmtpConfirmationModal.isPresent()).toBe(true);
        });

        describe('hitting the confirm button', () => {
          beforeEach(() => {
            mocks.push({
              request: {
                url: '/api/v0/e/Chef/notifications/smtp',
                method: 'DELETE'
              },
              response: {
                status: 204,
                body: {}
              }
            });

            mockApi(mocks);

            smtpSetupPage.removeButton.click();
            smtpSetupPage.confirmDeleteButton.click();
          });
          it('hides the Modal', () => {
            expect(smtpSetupPage.removeSmtpConfirmationModal.isPresent()).toBe(false);
          });

          it('removes the values of the config fields', () => {
            expect(smtpSetupPage.smtpHost.getAttribute('value')).toEqual('');
            expect(smtpSetupPage.smtpPort.getAttribute('value')).toEqual('');
            expect(smtpSetupPage.smtpLogin.getAttribute('value')).toEqual('');
            expect(smtpSetupPage.smtpPassword.getAttribute('value')).toEqual('');
            expect(smtpSetupPage.smtpSenderEmail.getAttribute('value')).toEqual('');
            expect(smtpSetupPage.smtpSenderName.getAttribute('value')).toEqual('');
          });
        });

        describe('hitting the cancel button', () => {
          beforeEach(() => {
            smtpSetupPage.removeButton.click();
            smtpSetupPage.cancelDeleteButton.click();
          });

          it('hides the Modal', () => {
            expect(smtpSetupPage.removeSmtpConfirmationModal.isPresent()).toBe(false);
          });

          it('retains the values of the config fields', () => {
            expect(smtpSetupPage.smtpHost.getAttribute('value')).toEqual('beep.boop');
            expect(smtpSetupPage.smtpPort.getAttribute('value')).toEqual('25');
            expect(smtpSetupPage.smtpLogin.getAttribute('value')).toEqual('roboty');
            expect(smtpSetupPage.smtpSenderEmail.getAttribute('value')).toEqual('name@bomb.com');
            expect(smtpSetupPage.smtpSenderName.getAttribute('value')).toEqual('namez');
          });
        });
      });
    });
  });
});
