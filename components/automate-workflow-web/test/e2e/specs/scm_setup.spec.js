import ScmSetupPage from '../page_objects/scm_setup.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('scm setup page', () => {

  let scmSetupPage;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {

    mockApi([
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
      },
      {
        request: {
          url: '/api/v0/e/Chef/users/grrm$',
          method: 'GET'
        },
        response: {
          status: 200
        }
      }
    ]);

    scmSetupPage = new ScmSetupPage();
  });

  describe('user not authorized', () => {
    beforeEach(() => {
      mockApi([
        authorizedLogin,
        {
          request: {
            url: '/api/v0/e/Chef/scm/bitbucket/servers$',
            method: 'GET'
          },
          response: {
            status: 403
          }
        }
      ]);
      scmSetupPage.get();
    });

    it('forbidden message div should be present', () => {
      expect(scmSetupPage.forbiddenText).toBeDisplayed();
    });
  });

  describe('user authorized', () => {
    describe('github setup', () => {

      describe('selecting github provider', () => {
        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: []
              }
            }
          ]);
          scmSetupPage.get();
          scmSetupPage.selectProvider('GitHub');
        });

        it('should show ctl command instructions (for now)', () => {
          expect(scmSetupPage.githubSetupFields).toBeDisplayed();
        });

        it('should not show any buttons (for now)', () => {
          expect(element(by.css('.form-controls')).isPresent()).toBe(true);
        });
      });
    });

    describe('bitbucket setup', () => {
      describe('selecting bitbucket provider', () => {
        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: []
              }
            }
          ]);
          scmSetupPage.get();
          scmSetupPage.selectProvider('Bitbucket');
        });

        it('should show bitbucket setup fields', () => {
          expect(scmSetupPage.bitbucketSetupFields).toBeDisplayed();
        });
      });

      describe('setting up bitbucket provider', () => {
        beforeEach(() => {
          mockApi([
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: []
              }
            }
          ]);
          scmSetupPage.get();
          scmSetupPage.selectProvider('Bitbucket');
        });

        describe('when setup succeeds', () => {
          beforeEach(() => {
            mockApi.clear();

            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                  method: 'POST'
                },
                response: {
                  status: 201
                }
              },
              {
                request: {
                  url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                  method: 'GET'
                },
                response: {
                  status: 200,
                  body: [
                    {
                      root_api_url: 'http://bitbucket.my.co',
                      user_id: 'user_id',
                      _links: {
                        self: {
                          href: 'https://delivery.shd.chef.co/api/v0/e/Chef/scm/bitbucket/servers/https%3A%2F%2F10.194.11.251'
                        }
                      }
                    }
                  ]
                }
              }
            ]);
          });

          // In UpgradeAdapter/ng12hybrid mode, Protractor may fail to detect the resolution
          // of outstanding promises (for example, with the $interval service) and time out.
          // This instructs Protractor to bypass that default (and usually desired) behavior.
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          it('should display a success message and change to update form', () => {
            scmSetupPage.bitbucketUrlField.sendKeys('http://bitbucket.url');
            scmSetupPage.bitbucketUserField.sendKeys('bitbucketUser');
            scmSetupPage.bitbucketPassField.sendKeys('bitbucketPass');
            scmSetupPage.saveButton.click();

            expect(element(by.css('.cd-flash.notify'))).toBeDisplayed();
            expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('http://bitbucket.my.co');
            expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('user_id');
            expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('********');
            expect(scmSetupPage.removeButton).toBeDisplayed();
            expect(scmSetupPage.updateButton).toBeDisplayed();
          });
        });

        describe('when setup fails', () => {

          beforeEach(() => {
            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                  method: 'POST'
                },
                response: {
                  status: 409
                }
              }
            ]);
          });

          // Don't wait for Protractor here (explanation above)
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          it('should display an error message', () => {
            scmSetupPage.bitbucketUrlField.sendKeys('http://bitbucket.url');
            scmSetupPage.bitbucketUserField.sendKeys('bitbucketUser');
            scmSetupPage.bitbucketPassField.sendKeys('bitbucketPass');
            scmSetupPage.saveButton.click();

            expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
          });
        });
      });

      describe('updating bitbucket provider', () => {
        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: [{
                  _links: { self: { href: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co'} },
                  root_api_url: 'http://bitbucket.my.co',
                  user_id: "user_id"
                }]
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers',
                method: 'GET'
              },
              response: {
                status: 200,
                body: [{
                  _links: { self: { href: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co'} },
                  root_api_url: 'http://bitbucket.my.co',
                  user_id: "user_id"
                }]
              }
            }
          ]);
          scmSetupPage.get();
          scmSetupPage.selectProvider('Bitbucket');
        });

        describe('when update succeeds', () => {
          beforeEach(() => {
            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                  method: 'PUT'
                },
                response: {
                  status: 200,
                  body: {
                    _links: { self: { href: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co'} },
                    root_api_url: 'http://bitbucket.my.co',
                    user_id: "user_id"
                  }
                }
              }
            ]);
          });

          // Don't wait for Protractor here (explanation above)
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          // Marked pending because of occasional failures
          xit('should display a success message', () => {
            scmSetupPage.bitbucketUrlField.clear();
            scmSetupPage.bitbucketUrlField.sendKeys('http://bitbucket.url');
            scmSetupPage.bitbucketUserField.clear();
            scmSetupPage.bitbucketUserField.sendKeys('bitbucketUser');
            scmSetupPage.bitbucketPassField.clear();
            scmSetupPage.bitbucketPassField.sendKeys('bitbucketPass');
            scmSetupPage.updateButton.click();

            expect(element(by.css('.cd-flash.notify'))).toBeDisplayed();
            expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('http://bitbucket.my.co');
            expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('user_id');
            expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('********');
            expect(scmSetupPage.removeButton).toBeDisplayed();
            expect(scmSetupPage.updateButton).toBeDisplayed();
          });
        });

        describe('when update fails', () => {
          beforeEach(() => {
            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                  method: 'PUT'
                },
                response: {
                  status: 500
                }
              }
            ]);
          });

          // Don't wait for Protractor here (explanation above)
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          it('should display an error message', () => {
            scmSetupPage.bitbucketUrlField.clear();
            scmSetupPage.bitbucketUrlField.sendKeys('http://bitbucket.url');
            scmSetupPage.bitbucketUserField.clear();
            scmSetupPage.bitbucketUserField.sendKeys('bitbucketUser');
            scmSetupPage.bitbucketPassField.clear();
            scmSetupPage.bitbucketPassField.sendKeys('bitbucketPass');
            scmSetupPage.updateButton.click();

            expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
            expect(scmSetupPage.removeButton).toBeDisplayed();
            expect(scmSetupPage.updateButton).toBeDisplayed();
          });
        });
      });

      describe('removing bitbucket provider', () => {
        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: [{
                  _links: { self: { href: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co'} },
                  root_api_url: 'http://bitbucket.my.co',
                  user_id: "user_id"
                }]
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  _links: { self: { href: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co'} },
                  root_api_url: 'http://bitbucket.my.co',
                  user_id: "user_id"
                }
              }
            }
          ]);
          scmSetupPage.get();
          scmSetupPage.selectProvider('Bitbucket');
        });

        describe('clicking remove link button', () => {

          beforeEach(() => {
            scmSetupPage.removeButton.click();
          });

          // Don't wait for Protractor here (explanation above)
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          it('should display a confirmation dialog', () => {
            expect(scmSetupPage.confirmationModal).toBeDisplayed();
          });

          describe('when clicking confirm', () => {
            describe('when remove succeeds', () => {
              beforeEach(() => {
                mockApi([
                  authorizedLogin,
                  {
                    request: {
                      url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                      method: 'DELETE'
                    },
                    response: {
                      status: 204
                    }
                  }
                ]);
              });

              it('should close the confirmation dialog and should display a success message', () => {
                scmSetupPage.modalConfirmButton.click();
                expect(scmSetupPage.confirmationModal.isPresent()).toBe(false);
                expect(element(by.css('.cd-flash.notify'))).toBeDisplayed();
                expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('');
                expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('');
                expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('');
                expect(scmSetupPage.saveButton).toBeDisplayed();
              });
            });

            describe('when remove fails due to existing bitbucket projects', () => {
              beforeEach(() => {
                mockApi([
                  authorizedLogin,
                  {
                    request: {
                      url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                      method: 'DELETE'
                    },
                    response: {
                      status: 412
                    }
                  }
                ]);
              });

              it('should close the confirmation dialog and should display an error message', () => {
                scmSetupPage.modalConfirmButton.click();
                expect(scmSetupPage.confirmationModal.isPresent()).toBe(false);
                expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
                expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('http://bitbucket.my.co');
                expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('user_id');
                expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('********');
                expect(scmSetupPage.updateButton).toBeDisplayed();
                expect(scmSetupPage.removeButton).toBeDisplayed();
              });
            });

            describe('when remove fails due to not finding bitbucket creds', () => {
              beforeEach(() => {
                mockApi([
                  authorizedLogin,
                  {
                    request: {
                      url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                      method: 'DELETE'
                    },
                    response: {
                      status: 404
                    }
                  }
                ]);
              });

              // Don't wait for Protractor here (explanation above)
              beforeEach(() => {
                browser.ignoreSynchronization = true;
              });

              afterEach(() => {
                browser.ignoreSynchronization = false;
              });

              it('should close the confirmation dialog and should display an error message', () => {
                scmSetupPage.modalConfirmButton.click();
                expect(scmSetupPage.confirmationModal.isPresent()).toBe(false);
                expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
                expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('http://bitbucket.my.co');
                expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('user_id');
                expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('********');
                expect(scmSetupPage.updateButton).toBeDisplayed();
                expect(scmSetupPage.removeButton).toBeDisplayed();
              });
            });

            describe('when remove fails', () => {
              beforeEach(() => {
                mockApi([
                  authorizedLogin,
                  {
                    request: {
                      url: '/api/v0/e/Chef/scm/bitbucket/servers/http%3A%2F%2Fbitbucket.my.co$',
                      method: 'DELETE'
                    },
                    response: {
                      status: 500
                    }
                  }
                ]);
              });

              // Don't wait for Protractor here (explanation above)
              beforeEach(() => {
                browser.ignoreSynchronization = true;
              });

              afterEach(() => {
                browser.ignoreSynchronization = false;
              });

              it('should close the confirmation dialog and should display an error message', () => {
                scmSetupPage.modalConfirmButton.click();
                expect(scmSetupPage.confirmationModal.isPresent()).toBe(false);
                expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
                expect(scmSetupPage.bitbucketUrlField.getAttribute('value')).toEqual('http://bitbucket.my.co');
                expect(scmSetupPage.bitbucketUserField.getAttribute('value')).toEqual('user_id');
                expect(scmSetupPage.bitbucketPassField.getAttribute('value')).toEqual('********');
                expect(scmSetupPage.updateButton).toBeDisplayed();
                expect(scmSetupPage.removeButton).toBeDisplayed();
              });
            });
          });

          describe('when clicking cancel', () => {
            beforeEach(() => {
              scmSetupPage.modalCancelButton.click();
            });

            it('should close the confirmation dialog', () => {
              expect(scmSetupPage.confirmationModal.isPresent()).toBe(false);
            });
          });
        });
      });
    });
  });
});
