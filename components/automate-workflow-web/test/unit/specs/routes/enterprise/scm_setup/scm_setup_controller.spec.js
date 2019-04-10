import ng from 'angular';
import 'angular-mocks';
import scmSetupController
  from '../../../../../../src/routes/enterprise/scm_setup/scm_setup_controller';
import removeScmConfirmationModal
  from  '../../../../../../src/common/ui/modal/remove_scm_confirmation_modal.html';
import featureFlags from '../../../../../../src/common/feature_flags/feature_flags'

describe('scmSetupController', () => {
  let scope, createController, scmProviders, scmSetup, Flash, Modal, $httpBackend;

  beforeEach(ng.mock.module(scmSetupController));

  beforeEach(inject(($controller, $rootScope, $compile, _$httpBackend_, _Modal_) => {
    scope = $rootScope.$new();
    $httpBackend = _$httpBackend_;
    Modal = _Modal_;

    Flash = jasmine.createSpyObj('Flash', ['notify', 'error']);

    scmProviders = [
      {
        name: 'Bitbucket',
        type: 'bitbucket',
        setupUrl: '/scm/bitbucket/servers'
      },
      {
        name: 'GitHub',
        type: 'github',
        setupUrl: '/scm/github/servers'
      }
    ];

    scmSetup = {bitbucket: undefined, github: undefined};

    createController = (scmSetup) => {
      return $controller('scmSetupController', {
        $scope: scope,
        Flash: Flash,
        Modal: Modal,
        scmProviders: scmProviders,
        scmSetup: scmSetup,
        authorized: true,
        featureFlags: featureFlags
      });
    };
  }));

  describe('setup', () => {

    it('should attach scmSetup object to scope to hold form data', () => {
      createController({bitbucket: undefined, github: undefined});
      expect(scope.scmSetup).toEqual(scmSetup);
    });

    it('should attach scmProviders to scope', () => {
      createController({bitbucket: undefined, github: undefined});
      expect(scope.scmProviders).toEqual(scmProviders);
    });

    it('should default the selectedProvider to the first scmProvider', () => {
      createController({bitbucket: undefined, github: undefined});
      expect(scope.selectedProvider).toEqual(scope.scmProviders[0]);
    });

    describe('if a Bitbucket config is set up', () => {
      let config = {
                      "root_api_url": "https://10.194.9.243",
                      "user_id": "automate",
                      "_links": {
                                  "self": {
                                            "href":"http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243"
                                          }
                                  }
                      };
      beforeEach(() => {
        createController({bitbucket: config, github: undefined});
      });

      it('assigns a dummy password to Bitbucket', () => {
        expect(scope.scmSetup.bitbucket.password).toEqual("********");
        expect(scope.scmSetup.github).toBe(undefined);
      })

    });

    describe('if a GitHub config is set up', () => {
      let config = {
                      "root_api_url": "https://10.194.9.243",
                      "user_id": "automate",
                      "_links": {
                                  "self": {
                                            "href":"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243"
                                          }
                                  }
                      };
      beforeEach(() => {
        createController({bitbucket: undefined, github: config});
      });

      it('assigns a dummy password to GitHub', () => {
        expect(scope.scmSetup.bitbucket).toBe(undefined);
        expect(scope.scmSetup.github.password).toEqual("********");
      })

    });

    describe('if a GitHub AND Bitbucket config is set up', () => {
      let config = {
                      "root_api_url": "https://10.194.9.243",
                      "user_id": "automate",
                      "_links": {
                                  "self": {
                                            "href":"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243"
                                          }
                                  }
                      };
      beforeEach(() => {
        createController({bitbucket: config, github: config});
      });

      it('assigns a dummy password to both', () => {
        expect(scope.scmSetup.bitbucket.password).toEqual("********");
        expect(scope.scmSetup.github.password).toEqual("********");
      })

    });
  });

  describe('editBitbucketPassword()', () => {
    let config = {
                    "root_api_url": "https://10.194.9.243",
                    "user_id": "automate",
                    "_links": {
                                "self": {
                                          "href":"http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243"
                                        }
                                }
                    };

    it('changes scope.scmSetup.bitbucket.passwordChange to true', () => {
      createController({bitbucket: config, github: undefined});
      scope.editBitbucketPassword();
      expect(scope.scmSetup.bitbucket.passwordChange).toBe(true);
    });
  });

  describe('editGithubPassword()', () => {
    let config = {
                    "root_api_url": "https://10.194.9.243",
                    "user_id": "automate",
                    "_links": {
                                "self": {
                                          "href":"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243"
                                        }
                                }
                    };
    it('changes $scope.scmSetup.github.passwordChange to true', () => {
      createController({bitbucket: undefined, github: config});
      scope.editGithubPassword();
      expect(scope.scmSetup.github.passwordChange).toBe(true);
    });
  });

  describe('selectProvider()', () => {

    it('should set the selectedProvider', () => {
      createController({bitbucket: undefined, github: undefined});

      let provider = scope.scmProviders[1];
      scope.selectProvider(provider);

      expect(scope.selectedProvider).toEqual(provider);
    });
  });

  describe('save()', () => {
    beforeEach(() => {
      createController({bitbucket: undefined, github: undefined});
      scope.scmSetupForm = { '$valid': true };
    });

    it('should POST to the correct URL for the selected scmProvider', () => {
      let params = {
                     "root_api_url": "https://10.194.9.243",
                     "user_id": "automate",
                     "password": "password"
                  };
      $httpBackend
        .expect('POST', scope.selectedProvider.setupUrl, params)
        .respond(201);

        $httpBackend
          .expect('GET', scope.selectedProvider.setupUrl)
          .respond(200, [{root_api_url: "http://bitbucket.my.co",
                         user_id: "user_id",
                         _links: { self: { href: 'http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243' } } }]);

      scope.scmSetup.bitbucket = {
                                  "root_api_url": "https://10.194.9.243",
                                  "user_id": "automate",
                                  "password": "password",
                                  "_links": {
                                             "self": {
                                                      "href":"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243"
                                                      }
                                              }
                                  };

      scope.editBitbucketPassword();
      scope.save();

      $httpBackend.flush();
    });

    describe('when server responds with success code it', () => {
      it('should show a success message and follow the location header', () => {
        $httpBackend
          .expectPOST(scope.selectedProvider.setupUrl)
          .respond(201, {}, {location: 'http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243'});

        $httpBackend
          .expectGET(scope.selectedProvider.setupUrl)
          .respond(200, [{root_api_url: "http://bitbucket.my.co",
                         user_id: "user_id",
                         _links: { self: { href: 'http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243' } } }]);

        scope.save();

        $httpBackend.flush();

        expect(Flash.notify).toHaveBeenCalledWith(
          'Success',
          'You have set up your Bitbucket SCM link successfully.'
        );

        let updatedScmSetup = scope.scmSetup[scope.selectedProvider.type];
        expect(updatedScmSetup._links.self.href).toEqual('http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243');
        expect(updatedScmSetup.root_api_url).toEqual("http://bitbucket.my.co");
        expect(updatedScmSetup.user_id).toEqual("user_id");
        expect(updatedScmSetup.password).toEqual("********");
      });
    });

    describe('when server responds with error code', () => {

      it('should show an error message', () => {
        $httpBackend
          .expectPOST(scope.selectedProvider.setupUrl)
          .respond((method, url, data, headers, params) => {
            return [500, {'message':'There was a problem setting up your Bitbucket SCM.'}];
          });

        scope.save();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was a problem setting up your Bitbucket SCM.'
        );
      });

      it ('should let the user know when the Bitbucket URL is not valid', () => {
        $httpBackend
          .expectPOST(scope.selectedProvider.setupUrl)
          .respond((method, url, data, headers, params) => {
            return [400, {'message':'Invalid URL'}];
          });

        scope.save();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Invalid URL'
        );
      });

      it ('should let the user know when Bitbucket credentials are invalid', () => {
        $httpBackend
          .expectPOST(scope.selectedProvider.setupUrl)
          .respond((method, url, data, headers, params) => {
            return [400, {'message':'Invalid credentials'}];
          });

        scope.save();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Invalid credentials'
        );
      });

      it ('should let the user know when Bitbucket URL is unreachable', () => {
        $httpBackend
          .expectPOST(scope.selectedProvider.setupUrl)
          .respond((method, url, data, headers, params) => {
            return [500, {'message':'Unreachable server'}];
          });

        scope.save();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Unreachable server'
        );
      });
    });
  });

  describe('update()', () => {

    beforeEach(() => {
      let config = {
                    "root_api_url": "https://10.194.9.243",
                    "user_id": "automate",
                    "_links": {
                               "self": {
                                        "href":"http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243"
                                        }
                               }
                    };
      let gHconfig = {
                      "root_api_url": "https://10.194.9.243",
                      "user_id": "automate",
                      "_links": {
                                 "self": {
                                          "href":"http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243"
                                          }
                                  }
                      };

      createController({bitbucket: config, github: gHconfig});
      scope.scmSetupForm = { '$valid': true };
    });

    it('should PUT to the correct URL for the selected scmData', () => {
      $httpBackend
        .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
        .respond(200);

      scope.update();

      $httpBackend.flush();
    });

    describe('if the password is changed', () => {
      it('PUTS to the URL with the password field', () => {
        let params = {
                       "root_api_url": "https://10.194.9.243",
                       "user_id": "automate",
                       "password": "password"
                    };
        $httpBackend
          .expect('PUT', 'http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243', params)
          .respond(200);

          scope.scmSetup.bitbucket.password = "password";

          scope.editBitbucketPassword();
          scope.update();

          $httpBackend.flush();
      });
    });

    describe('if the password is not changed', () => {

      describe('and it is a bitbucket scm', () => {

        it('PUTS to the URL without the password field', () => {
          let params = {
                        root_api_url: "https://10.194.9.243",
                        user_id: "automate"
                       }
          $httpBackend
            .expect('PUT', 'http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243', params)
            .respond(200);

            scope.update();

            $httpBackend.flush();
        });
      });

      describe('and it is a github scm', () => {
        beforeEach(() => {
          scope.selectProvider({
                            name: "GitHub",
                            type: "github",
                            setupUrl: "/api/v0/e/cd/scm/github/servers"
                          });
        });

        it('PUTS to the URL without the password field', () => {
          let params = {
                        root_api_url: "https://10.194.9.243",
                        user_id: "automate"
                       }
          $httpBackend
            .expect('PUT', 'http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243', params)
            .respond(200);

            scope.update();

            $httpBackend.flush();
        });

        describe('and the user changes the bitbucket pw but does not update bitbucket information', () => {

          it('PUTS to the github URL without the password field', () => {
            let params = {
                          root_api_url: "https://10.194.9.243",
                          user_id: "automate"
                         }
            $httpBackend
              .expect('PUT', 'http://192.168.33.66/api/v0/e/cd/scm/github/servers/https%3A%2F%2F10.194.9.243', params)
              .respond(200);

              scope.editBitbucketPassword();
              scope.update();

              $httpBackend.flush();
          });

        });
      });

    });

    describe('when server responds with success code it', () => {
      it('should show a success message and update scope', () => {
        $httpBackend
          .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond(200, {root_api_url: "http://bitbucket.my.co",
                         user_id: "user_id",
                         _links: { self: { href: 'http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243' } } });

        scope.update();

        $httpBackend.flush();

        expect(Flash.notify).toHaveBeenCalledWith(
          'Success',
          'You have updated your Bitbucket SCM successfully.'
        );

        let updatedScmSetup = scope.scmSetup[scope.selectedProvider.type];
        expect(updatedScmSetup._links.self.href).toEqual('http://delivery.my.co/api/v0/e/cd/scm/bitbucket/servers/http%3A%2F%2F10.194.9.243');
        expect(updatedScmSetup.root_api_url).toEqual("http://bitbucket.my.co");
        expect(updatedScmSetup.user_id).toEqual("user_id");
        expect(updatedScmSetup.password).toEqual("********");

        expect(scope.scmSetup.bitbucket.passwordChange).toBe(false);
      });
    });

    describe('when server responds with error code', () => {

      it('should show an error message', () => {
        $httpBackend
          .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [500, {'message':'There was a problem updating your SCM.'}];
          });

        scope.update();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was a problem updating your SCM.'
        );
      });

      it ('should let the user know when the Bitbucket URL is not valid', () => {
        $httpBackend
          .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [400, {'message':'Invalid URL'}];
          });

        scope.update();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Invalid URL'
        );
      });

      it ('should let the user know when Bitbucket credentials are invalid', () => {
        $httpBackend
          .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [400, {'message':'Invalid credentials'}];
          });

        scope.update();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Invalid credentials'
        );
      });

      it ('should let the user know when Bitbucket URL is unreachable', () => {
        $httpBackend
          .expectPUT('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [500, {'message':'Unreachable server'}];
          });

        scope.update();

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Unreachable server'
        );
      });
    });
  });

  describe('delete()', () => {
    beforeEach(() => {
      createController({bitbucket: undefined, github: undefined});
      spyOn(Modal, 'open');
    });

    it('should open confirmation Modal', () => {
      scope.delete();
      expect(Modal.open).toHaveBeenCalledWith(
        'Confirmation',
        removeScmConfirmationModal,
        'red-modal',
        scope
      );
    });
  });

  describe('doDelete()', () => {
    let config = {
                    "root_api_url": "https://10.194.9.243",
                    "user_id": "automate",
                    "_links": {
                                "self": {
                                          "href":"http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243"
                                        }
                                }
                    };

    beforeEach(() => {
      createController({bitbucket: config, github: undefined});
      scope.scmSetupForm = { '$valid': true };
      spyOn(Modal, 'close');
    });

    it('should close the confirmation Modal and DELETE to the correct URL for the selected scmData', () => {
      $httpBackend
        .expectDELETE('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
        .respond(204);

      scope.doDelete({bitbucket: config, github: undefined});
      expect(Modal.close).toHaveBeenCalled();

      $httpBackend.flush();
    });

    describe('when server responds with success code it', () => {
      it('should show a success message and update scope', () => {
        $httpBackend
          .expectDELETE('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond(204);

        scope.doDelete({bitbucket: config, github: undefined});

        $httpBackend.flush();

        expect(Flash.notify).toHaveBeenCalledWith(
          'Success',
          'You have removed the Bitbucket SCM link successfully.'
        );

        let updatedScmSetup = scope.scmSetup[scope.selectedProvider.type];
        expect(updatedScmSetup._links).toBe(undefined);
        expect(updatedScmSetup.root_api_url).toBe(undefined);
        expect(updatedScmSetup.user_id).toBe(undefined);
        expect(updatedScmSetup.password).toBe(undefined);
      });
    });

    describe('when server responds with error code', () => {

      it('should show an error message when there are still projects linked to bitbucket', () => {
        $httpBackend
          .expectDELETE('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [412, {'message':'Some Automate projects are connected to Bitbucket repositories. Please remove those links before removing the SCM link to Bitbucket.'}];
          });

        scope.doDelete({bitbucket: config, github: undefined});

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'Some Automate projects are connected to Bitbucket repositories. Please remove those links before removing the SCM link to Bitbucket.'
        );
      });

      it ('should show an error message when there is any other error', () => {
        $httpBackend
          .expectDELETE('http://192.168.33.66/api/v0/e/cd/scm/bitbucket/servers/https%3A%2F%2F10.194.9.243')
          .respond((method, url, data, headers, params) => {
            return [500, {'message':'There was a problem removing the Bitbucket SCM link.'}];
          });

        scope.doDelete({bitbucket: config, github: undefined});

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was a problem removing the Bitbucket SCM link.'
        );
      });
    });
  });

  describe('closeModal()', () => {
    beforeEach(() => {
      createController({bitbucket: undefined, github: undefined});
      spyOn(Modal, 'close');
    });

    it('should close confirmation Modal', () => {
      scope.closeModal();
      expect(Modal.close).toHaveBeenCalled();
    });
  });

  describe('formattedType(type)', () => {
    beforeEach(() => {
      createController({bitbucket: undefined, github: undefined});
    });

    it('returns "Bitbucket" if the type="bitbucket"', () => {
      expect(scope.formattedType("bitbucket")).toEqual("Bitbucket");
    });

    it('returns "GitHub" if the type="github"', () => {
      expect(scope.formattedType("github")).toEqual("GitHub");
    });
  });
});
