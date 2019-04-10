import SamlSetupPage from '../page_objects/saml_setup.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('saml setup page', () => {
  let samlSetupPage, mocks;

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
    samlSetupPage = new SamlSetupPage();
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
      });
      mockApi(mocks);
      samlSetupPage.get();
    });

    it('shows the forbidden message', () => {
      expect(samlSetupPage.forbiddenText.isPresent()).toBe(true);
    });
  });

  describe('when the user is an enterprise admin', () => {


    describe('and saml has not been not set up', () => {

      beforeEach(() => {
        mocks.push( {
          request: {
            url: '/api/v0/e/Chef/authz/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/saml/config',
            method: 'GET'
          },
          response: {
            status: 404
          }
        });

        mockApi(mocks);
        samlSetupPage.get();
      });

      it('shows the setup form', () => {
        expect(samlSetupPage.samlSetupFields.isPresent()).toBe(true);
      });

      it('shows the default settings', () => {
        expect(samlSetupPage.samlSSOLogin.getAttribute('value')).toEqual('');
        expect(samlSetupPage.samlIdPUrl.getAttribute('value')).toEqual('');
        expect(samlSetupPage.samlNameIdPolicy.getText()).toEqual('Default (No Policy)');
        expect(samlSetupPage.samlCert.getAttribute('value')).toEqual('');
        expect(samlSetupPage.samlDefaultRoles.isPresent()).toBe(true);
      });

      it('has manual configuration selected', () => {
        expect(samlSetupPage.samlManualCheckbox).toBeChecked();
        expect(samlSetupPage.samlMetadataCheckbox).not.toBeChecked();
      });

      it('has the save button disabled', () => {
        expect(samlSetupPage.saveButton.isEnabled()).toBe(false);
      });

      it('does not show a confirmation modal when a user saves', ()  => {
        expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
      });
    });

    describe('and an invalid url is input', () => {
      describe('with a manual configuration', () => {
        beforeEach(() => {
          samlSetupPage.samlSSOLogin.sendKeys('invalid');
        });

        it('has the save button disabled', () => {
          expect(samlSetupPage.samlManualCheckbox).toBeChecked();
          expect(samlSetupPage.saveButton.isEnabled()).toBe(false);
        });
      });

      describe('with a metadata configuration', () => {
        beforeEach(() => {
            samlSetupPage.samlMetadataCheckbox.click()
            samlSetupPage.samlMetadataUrl.sendKeys('invalid');
        });

        it('has the save button disabled', () => {
          expect(samlSetupPage.samlMetadataCheckbox).toBeChecked();
          expect(samlSetupPage.saveButton.isEnabled()).toBe(false);
        });
      });
    });

    describe('and a manual saml configuration exists', () => {
      beforeEach(() => {
        mocks.push( {
          request: {
            url: '/api/v0/e/Chef/authz/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/saml/config',
            method: 'GET'
          },
          response: {
            status: 200,
            body:{
              sso_login_url: 'http://idp.com/login',
              sso_binding: 'HTTP-Redirect',
              idp_url: 'http://idp.com',
              cert: 'secrets',
              name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
            }
          }
        });

        mockApi(mocks);
        samlSetupPage.get();
      });

      it('has manual configuration selected', () => {
        expect(samlSetupPage.samlManualCheckbox).toBeChecked();
        expect(samlSetupPage.samlMetadataCheckbox).not.toBeChecked();
      });

      it('populates the fields with the configuration values', () =>{
        expect(samlSetupPage.samlSSOLogin.getAttribute('value')).toEqual('http://idp.com/login');
        expect(samlSetupPage.samlIdPUrl.getAttribute('value')).toEqual('http://idp.com');
        expect(samlSetupPage.samlNameIdPolicy.getText()).toEqual('Entity');
        expect(samlSetupPage.samlCert.getAttribute('value')).toEqual('secrets');
      });

      it('has metadata inputs disabled', () => {
        expect(samlSetupPage.samlMetadataUrl.isEnabled()).toBe(false);
      });

      it('has the save button enabled', () => {
        expect(samlSetupPage.saveButton.isEnabled()).toBe(true);
      });

      describe('and the user clicks the metadata checkbox', () => {
        beforeEach(() => {
          samlSetupPage.samlMetadataCheckbox.click();
        });

        it('disables the manual input fields', () => {
          expect(samlSetupPage.samlSSOLogin.isEnabled()).toBe(false);
          expect(samlSetupPage.samlIdPUrl.isEnabled()).toBe(false);
          expect(samlSetupPage.samlCert.isEnabled()).toBe(false);
        });

        it('enables the metadata input fields', () => {
          expect(samlSetupPage.samlMetadataUrl.isEnabled()).toBe(true);
        });

        it('retains the value in the manual url field', () => {
          expect(samlSetupPage.samlSSOLogin.getAttribute('value')).toEqual('http://idp.com/login');
          expect(samlSetupPage.samlIdPUrl.getAttribute('value')).toEqual('http://idp.com');
          expect(samlSetupPage.samlCert.getAttribute('value')).toEqual('secrets');
        });

        describe('and a user edits the metadata url field and saves', () => {
          beforeEach(() => {
            samlSetupPage.samlMetadataUrl.sendKeys('https://idp.com/metadata.xml');
            samlSetupPage.saveButton.click();
          });

          it('shows a confirmation modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(true);
          });

          describe('hitting the confirm button', () => {
            beforeEach(() => {
              mocks.push({
                request: {
                  url: '/api/v0/e/Chef/saml/config',
                  method: 'PUT'
                },
                response: {
                  status: 201,
                  body: {
                    metadata_url: 'https://idp.com/metadata.xml',
                    name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
                  }
                }
              });

              mockApi(mocks);
              samlSetupPage.confirmEditButton.click();
            });

            it('hides the Modal', () => {
              expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
            });
          });
        });
      });

      describe('and a user edits a field and saves', () => {
        beforeEach(() => {
            samlSetupPage.samlSSOLogin.sendKeys('foobear');
            samlSetupPage.saveButton.click();
        });

        it('shows a confirmation modal', () => {
          expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(true);
        });

        describe('hitting the confirm button', () => {
          beforeEach(() => {
            mocks.push({
              request: {
                url: '/api/v0/e/Chef/saml/config',
                method: 'PUT'
              },
              response: {
                status: 201,
                body: {
                    sso_login_url: 'http://idp.com/loginfoobear',
                    sso_binding: 'HTTP-Redirect',
                    idp_url: 'http://idp.com',
                    cert: 'secrets',
                    name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
                }
              }
            });

            mockApi(mocks);
              samlSetupPage.confirmEditButton.click();
          });
          it('hides the Modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
          });

        });

        describe('hitting the cancel button', () => {
          beforeEach(() => {
            samlSetupPage.cancelEditButton.click();
          });

          it('hides the Modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
          });

          it('retains the values of the config fields', () => {
            expect(samlSetupPage.samlSSOLogin.getAttribute('value')).toEqual('http://idp.com/loginfoobear');
            expect(samlSetupPage.samlIdPUrl.getAttribute('value')).toEqual('http://idp.com');
            expect(samlSetupPage.samlNameIdPolicy.getText()).toEqual('Entity');
            expect(samlSetupPage.samlCert.getAttribute('value')).toEqual('secrets');
          });

          it('retains the manual configuration selected', () => {
            expect(samlSetupPage.samlManualCheckbox).toBeChecked();
            expect(samlSetupPage.samlMetadataCheckbox).not.toBeChecked();
          });
        });
      });
    });

    describe('and a metadata saml configuration exists', () => {
      beforeEach(() => {
        mocks.push( {
          request: {
            url: '/api/v0/e/Chef/authz/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/saml/config',
            method: 'GET'
          },
          response: {
            status: 200,
            body:{
              metadata_url: 'http://idp.com/metadata.xml',
              name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
            }
          }
        });

        mockApi(mocks);
        samlSetupPage.get();
      });

      it('has metadata configuration selected', () => {
        expect(samlSetupPage.samlManualCheckbox).not.toBeChecked();
        expect(samlSetupPage.samlMetadataCheckbox).toBeChecked();
      });

      it('populates the fields with the configuration values', () =>{
        expect(samlSetupPage.samlMetadataUrl.getAttribute('value')).toEqual('http://idp.com/metadata.xml');
        expect(samlSetupPage.samlNameIdPolicy.getText()).toEqual('Entity');
      });

      it('has the save button enabled', () => {
        expect(samlSetupPage.saveButton.isEnabled()).toBe(true);
      });

      describe('and the user clicks the manual checkbox', () => {
        beforeEach(() => {
            samlSetupPage.samlManualCheckbox.click()
        });

        it('disables the metdata input fields', () => {
          expect(samlSetupPage.samlMetadataUrl.isEnabled()).toBe(false);
        });

        it('enables the manual input fields', () => {
          expect(samlSetupPage.samlSSOLogin.isEnabled()).toBe(true);
          expect(samlSetupPage.samlIdPUrl.isEnabled()).toBe(true);
          expect(samlSetupPage.samlCert.isEnabled()).toBe(true);
        });

        it('retains the value in the metdata url field', () => {
          expect(samlSetupPage.samlMetadataUrl.getAttribute('value')).toEqual('http://idp.com/metadata.xml');
        });

        describe('and a user edits the manual configuration fields and saves', () => {
          beforeEach(() => {
            samlSetupPage.samlSSOLogin.sendKeys('http://idp.com/login');
            samlSetupPage.samlIdPUrl.sendKeys('example:urn:foo');
            samlSetupPage.samlCert.sendKeys('cert');
            samlSetupPage.saveButton.click();
          });

          it('shows a confirmation modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(true);
          });

          describe('hitting the confirm button', () => {
            beforeEach(() => {
              mocks.push({
                request: {
                  url: '/api/v0/e/Chef/saml/config',
                  method: 'PUT'
                },
                response: {
                  status: 201,
                  body: {
                    sso_login_url: 'http://idp.com/login',
                    sso_binding: 'HTTP-Redirect',
                    idp_url: 'example:urn:foo',
                    cert: 'cert',
                    name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
                  }
                }
              });

              mockApi(mocks);
              samlSetupPage.confirmEditButton.click();
            });

            it('hides the Modal', () => {
              expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
            });
          });
        });
      });

      describe('and a user edits a field and saves', () => {
        beforeEach(() => {
            samlSetupPage.samlMetadataUrl.sendKeys('foobear');
            samlSetupPage.saveButton.click();
        });

        it('shows a confirmation modal', () => {
          expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(true);
        });

        describe('hitting the confirm button', () => {
          beforeEach(() => {
            mocks.push({
              request: {
                url: '/api/v0/e/Chef/saml/config',
                method: 'PUT'
              },
              response: {
                status: 201,
                body: {
                    metadata_url: 'http://idp.com/metadata.xmlfoobear',
                    name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
                }
              }
            });

            mockApi(mocks);
              samlSetupPage.confirmEditButton.click();
          });

          it('hides the Modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
          });
        });

        describe('hitting the cancel button', () => {
          beforeEach(() => {
            samlSetupPage.cancelEditButton.click();
          });

          it('hides the Modal', () => {
            expect(samlSetupPage.overwriteSamlConfirmationModal.isPresent()).toBe(false);
          });

          it('retains the values of the config fields', () => {
            expect(samlSetupPage.samlMetadataUrl.getAttribute('value')).toEqual('http://idp.com/metadata.xmlfoobear');
            expect(samlSetupPage.samlNameIdPolicy.getText()).toEqual('Entity');
          });
        });
      });
    });
  });
});
