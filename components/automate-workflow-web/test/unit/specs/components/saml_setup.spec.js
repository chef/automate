import ng from 'angular';
import 'angular-mocks';
import samlSetupComponent from '../../../../src/components/saml_setup/saml_setup';


describe('SamlSetupComponent', () => {
  let scope, element, isolateScope, session, $httpBackend, $http, Modal;

  beforeEach(ng.mock.module(samlSetupComponent));

  beforeEach(ng.mock.module(samlSetupComponent, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  function createDirective(saml) {
    return inject(($compile, $rootScope, _$httpBackend_, _Modal_, _$http_) => {
      scope = $rootScope.$new();
      $httpBackend = _$httpBackend_;
      Modal = _Modal_;
      $http = _$http_

      scope.saml = saml;

      element = $compile(ng.element('<cd-saml-setup saml="saml">'))(scope);
      isolateScope = element.isolateScope();
      scope.$digest();
    });
  };

  describe('when there is no preexisting config', () => {
    beforeEach(() => {
      let saml = {
        authorized: true,
        config: null,
      };
      createDirective(saml);
    });

    describe('on init', () => {

      describe('scope.config', () => {

        it('defaults to manual configuration', () => {
          expect(isolateScope.isManual).toBe(true);
        });

        it('is assigned no config defaults except name_id is default', () => {
          expect(isolateScope.config.sso_login_url).toBeUndefined();
          expect(isolateScope.config.sso_binding).toBeUndefined();
          expect(isolateScope.config.idp_url).toBeUndefined();
          expect(isolateScope.config.cert).toBeUndefined();
          expect(isolateScope.config.name_id).toBe('default');
        });

        it('assigns all roles in scope.roles to be false except the default observer', () => {
          expect(isolateScope.roles.observer).toBe(true);
          expect(isolateScope.roles.committer).toBe(false);
          expect(isolateScope.roles.reviewer).toBe(false);
          expect(isolateScope.roles.shipper).toBe(false);
        })
      });
    });

    describe('scope.configExists', () => {
      it('is false', () => {
        expect(isolateScope.configExists).toBe(false);
      });
    });

    describe('on save' , () => {

      it('does not show the confirmation modal', () => {
        spyOn(Modal, 'open');
        isolateScope.save();
        expect(Modal.open).not.toHaveBeenCalled();
      });

      describe('scope.determineDefaultRoles()', () => {
        it('sets scope.config.default_roles to an empty array if all values of scope.roles are false', () =>{
          isolateScope.roles = {'admin': false, 'committer': false, 'reviewer': false, 'shipper': false, 'observer': false};

          isolateScope.determineDefaultRoles()
          expect(isolateScope.config.default_roles).toEqual([]);
        });
      });

      describe('when unsuccessful using manual config', () => {
        let config = {
          sso_login_url : 'http://idp.com/login',
          sso_binding : 'HTTP-Redirect',
          idp_url : 'http://idp.com',
          cert : 'SECRETS',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity',
        };

        beforeEach(() => {
          isolateScope.config = config;
          $httpBackend
            .expect('PUT', apiUrl(`/saml/config`), config)
            .respond(400, {
              'error':'bad_request'
            });
        });

        it('displays flash message indicating failure', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'error');

          isolateScope.save();
          $httpBackend.flush();

          expect(Flash.error).toHaveBeenCalledWith('Error saving SAML configuration', undefined);
        }));
      });

      describe('when unsuccessful using metadata config', () => {
        let config = {
          metadata_url : 'http://idp.com/metadata.xml',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
        };

        beforeEach(() => {
          isolateScope.config = config;
          isolateScope.isManual = false;
          $httpBackend
            .expectPUT(apiUrl(`/saml/config`), config)
            .respond(400, {
              'error':'bad_request',
              'message':'Some reason.'
            });
        });

        it('displays flash message indicating failure', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'error');

          isolateScope.save();
          $httpBackend.flush();

          expect(Flash.error).toHaveBeenCalledWith('Error saving SAML configuration', 'Please try configuring the SAML metadata manually. Some reason.');
        }));
      });

      describe('when unsuccessful with the default roles', () => {
        let config = {
          metadata_url : 'http://idp.com/metadata.xml',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity',
          default_roles: []
        };

        let errorMessage = 'Please select at least one default role.';

        beforeEach(() => {
          isolateScope.config = config;
          isolateScope.isManual = true;
          $httpBackend
            .expectPUT(apiUrl(`/saml/config`), config)
            .respond(400, {
              'error': 'bad_request',
              'message': errorMessage
            });
        });

        it('displays flash message indicating failure', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'error');

          isolateScope.save();
          $httpBackend.flush();

          expect(Flash.error).toHaveBeenCalledWith('Error saving SAML configuration', errorMessage);
        }));
      });

      describe('when all fields for manual config are supplied', () => {
        let config = {
          sso_login_url : 'http://idp.com/login',
          sso_binding : 'HTTP-Redirect',
          idp_url : 'http://idp.com',
          cert : 'SECRETS',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity',
        };

        it('makes put request to /saml/config with config and notifies of success', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'notify');
          isolateScope.config = config;

          $httpBackend
            .expectPUT(apiUrl(`/saml/config`), config)
            .respond(201);

          isolateScope.save();

          $httpBackend.flush();
          expect(Flash.notify).toHaveBeenCalled();
        }));
      });

      describe('when all fields for metadata config are supplied', () => {
        let config = {
          metadata_url : 'http://idp.com/metadata.xml',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
        };

        it('makes put request to /saml/config with config and notifies of success', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'notify');
          isolateScope.config = config;

          $httpBackend
            .expectPUT(apiUrl(`/saml/config`), config)
            .respond(201);

          isolateScope.save();

          $httpBackend.flush();
          expect(Flash.notify).toHaveBeenCalled();
        }));
      });
    });
  });

  describe('when there is a preexisting manual config', () => {
    describe('scope.configExists', () => {
      it('is true', () => {
        expect(isolateScope.configExists).toBe(true);
      });
    });

    beforeEach(() => {
      let saml = {
        authorized: true,
        config: {
          sso_login_url : 'http://idp.com/login',
          sso_binding : 'HTTP-Redirect',
          idp_url : 'http://idp.com',
          cert : 'SECRETS',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity',
          default_roles: ['admin']
          }
      };
      createDirective(saml);
    });

    describe('on init', () => {

      describe('scope', () => {

        it('has manual configuration selected', () => {
          expect(isolateScope.isManual).toBe(true);
        });
      });

      describe('scope.config', () => {

        it('is assigned the existing config', () => {
          expect(isolateScope.config.sso_login_url).toBe('http://idp.com/login');
          expect(isolateScope.config.sso_binding).toBe('HTTP-Redirect');
          expect(isolateScope.config.idp_url).toBe('http://idp.com');
          expect(isolateScope.config.cert).toBe('SECRETS');
          expect(isolateScope.config.name_id).toBe('urn:oasis:names:tc:SAML:2.0:nameid-format:entity');
        });

        it('sets up the roles correctly', () => {
          expect(isolateScope.roles.admin).toBe(true);
          expect(isolateScope.roles.observer).toBe(false);
        });
      });
    });

    describe('on save' , () => {

      it('a modal comes up to make sure you wish to overwrite the config', () => {
        spyOn(Modal, 'open');
        isolateScope.save();
        expect(Modal.open).toHaveBeenCalled();
      });

      describe('and the cancel button is clicked', () => {

        it('hides the confirmation modal', () => {
          spyOn(Modal, 'close');
          isolateScope.closeModal();
          expect(Modal.close).toHaveBeenCalled();
        });

        it('does not update the SAML config via the API', () => {
          spyOn($http, 'put');
          isolateScope.closeModal();
          expect($http.put).not.toHaveBeenCalled();
        });
      });

      describe('and the confirm button is clicked', () => {

        it('hides the confirmation modal', () => {
          spyOn(Modal, 'close');
          isolateScope.doSave();
          expect(Modal.close).toHaveBeenCalled();
        });
      });
    });

    describe('on delete', () => {
      it('a modal comes up to make sure you wish to delete the config', () => {
        spyOn(Modal, 'open');
        isolateScope.confirmDelete();
        expect(Modal.open).toHaveBeenCalled();
      });

      describe('and the cancel button is clicked', () => {
        it('does not delete the SAML config via the API', () => {
          spyOn($http, 'delete');
          isolateScope.closeModal();
          expect($http.delete).not.toHaveBeenCalled();
        });
      });

      describe('and the confirm button is clicked', () => {
        beforeEach(() => {
          $httpBackend
            .when('DELETE', apiUrl('/saml/config'))
            .respond(204);
          $httpBackend
            .expectDELETE(apiUrl('/saml/config'));
        });

        it('deletes the SAML config', inject((Flash) => {
          Flash.register(() => {});
          isolateScope.doDelete();
          $httpBackend.flush();
        }));

        it('notifies the user via the flash message', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'notify');
          isolateScope.doDelete();
          $httpBackend.flush();
          expect(Flash.notify).toHaveBeenCalled();
        }));

        it('sets the current config to be empty', inject((Flash) => {
          Flash.register(() => {});
          isolateScope.doDelete();
          $httpBackend.flush();
          expect(isolateScope.config).toEqual({});
        }));

        it('sets the state of the config page to a non-existant config', inject((Flash) => {
          Flash.register(() => {});
          isolateScope.doDelete();
          $httpBackend.flush();
          expect(isolateScope.configExists).toEqual(false);
        }));

        it ('hides the confirmation modal', () => {
          spyOn(Modal, 'close');
          isolateScope.doDelete();
          expect(Modal.close).toHaveBeenCalled();
        });
      });
    });
  });

  describe('when there is a preexisting metadata config', () => {

    beforeEach(() => {
      let saml = {
        authorized: true,
        config: {
          metadata_url: 'https://idp.com/saml/metadata.xml',
          name_id: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'
          }
      };
      createDirective(saml);
    });

    describe('scope.configExists', () => {
      it('is true', () => {
        expect(isolateScope.configExists).toBe(true);
      });
    });

    describe('on init', () => {
      describe('scope', () => {

        it('has manual configuration selected', () => {
          expect(isolateScope.isManual).toBe(false);
        });
      });
    });
  });

  describe('scope.toggleRole(role)', () => {
    it('toggles the role on scope.roles', () => {
      isolateScope.roles.observer = true;
      isolateScope.roles.reviewer = false;
      isolateScope.toggleRole('observer');
      isolateScope.toggleRole('reviewer');
      expect(isolateScope.roles.observer).toBe(false);
      expect(isolateScope.roles.reviewer).toBe(true);
    })
  });
});
