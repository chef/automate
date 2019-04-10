import ng from 'angular';
import 'angular-mocks';
import User from '../../../../src/common/models/user';
import UserCardComponent from '../../../../src/components/user_card/user_card';
import Modal from '../../../../src/common/ui/modal/modal';

describe('userCard', () => {

  let element, httpBackend, isolateScope, scope, Modal, $http;

  beforeEach(ng.mock.module(User, ($provide) => {
    $provide.value('ApiUrl', (path) => {
      return `/api/v0/e/foocorp${path}`;
    });
  }));

  beforeEach(ng.mock.module(User));
  beforeEach(ng.mock.module(UserCardComponent));

  beforeEach(inject((User, $compile, $httpBackend, $rootScope, _Modal_, _$http_) => {
    scope = $rootScope.$new();
    scope.user = User.$buildRaw({ name: "someone" });
    httpBackend = $httpBackend;
    Modal = _Modal_;
    $http = _$http_;

    element = $compile(ng.element('<cd-user-card user="user"></cd-user-card>'))(scope);
    createDirective();
    isolateScope = element.isolateScope();
  }));

  function createDirective() {
    return inject(($compile) => {
      element = $compile(ng.element(`
        <div
          cd-user-card
          user="user">
        </div>`))(scope);
      scope.$digest();
    });
  }

  describe('as an administrator', () => {

    describe('editing an existing internal user', () => {

      beforeEach(() => {
      httpBackend
        .when('GET', '/api/v0/e/foocorp/users/someone')
        .respond({
          "name": "someone",
          "first": "Some",
          "last": "One",
          "email": "someone@foo.com",
          "ssh_pub_key": "some-key",
          "user_type" : "internal",
          "_links": {
            "self": {
              "href": "/api/v0/e/foocorp/users/someone"
            },
            "change-password": {
              "href": "/api/v0/e/foocorp/internal-users/someone/change-password"
            }
          }
        });

        httpBackend
          .when('GET', '/api/v0/e/foocorp/authz/users/someone')
          .respond({
            "observer": ["enterprise"]
          });
        isolateScope.toggleEditForm();
        httpBackend.flush();
      });

      it('shows the edit form', () => {
        expect(isolateScope.showEditForm).toEqual(true);
      });

      it('scope.user.userType should initialize as "internal"', () => {
        expect(isolateScope.showEditForm).toEqual(true);
        expect(isolateScope.user.userType).toEqual('internal');
        expect(isolateScope.currentUserType).toEqual('internal');
        expect(isolateScope.internalDisabled).toEqual(false);
        expect(isolateScope.samlDisabled).toEqual(false);
        expect(isolateScope.externalDisabled).toEqual(false);
      });

      it('displays role-selection options', () => {
        expect(element[0].querySelectorAll('.roles').length).toBe(1);
      });

      it('empties the password value', () => {
        expect(scope.user.password).toBeNull();
      });

      describe('modifying the password field', () => {

        beforeEach(() => {
          scope.user.password = 'some-characters';
        });

        describe('and then closing the form', () => {

          beforeEach(() => {
            isolateScope.toggleEditForm();
          });

          it('empties the password value', () => {
            expect(scope.user.password).toBeNull();
          });
        });
      });

      describe('modifying the name field', () => {
        let user = {
          "name":"someone-else",
          "first": "Some",
          "last": "One",
          "email": "someone@foo.com",
          "ssh_pub_key": "some-key",
          "user_type" : "internal"
        }

        beforeEach(() => {
          isolateScope.user.name = user.name;
          isolateScope.saveAndClose();
        });

        it('calls the proper API endpoints and user sees a Flash notification', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'notify');

          httpBackend
            .expect('PUT', '/api/v0/e/foocorp/users/someone', user)
            .respond(201);
          httpBackend
            .expect('POST', '/api/v0/e/foocorp/authz/users/someone-else', {"set":["observer"]})
            .respond(201);
          httpBackend
            .expect('GET', '/api/v0/e/foocorp/users/someone-else')
            .respond({
              "name": "someone-else",
              "first": "Some",
              "last": "One",
              "email": "someone@foo.com",
              "ssh_pub_key": "some-key",
              "user_type" : "internal",
              "_links": {
                "self": {
                  "href": "/api/v0/e/foocorp/users/someone-else"
                },
                "change-password": {
                  "href": "/api/v0/e/foocorp/internal-users/someone-else/change-password"
                }
              }
            });
          httpBackend.flush();

          expect(Flash.notify).toHaveBeenCalled();
        }));

        describe('when updating the name fails', () => {

          it('calls the proper API endpoint and user sees a Flash notification', inject((Flash) => {
            Flash.register(() => {});
            spyOn(Flash, 'error');
            httpBackend
              .expect('PUT', '/api/v0/e/foocorp/users/someone', user)
              .respond(400, {
                "error":"bad_request",
                "message":"Some reason"
              });
            httpBackend.flush();

            expect(Flash.error).toHaveBeenCalledWith('Error Updating user', 'Some reason');
          }));

        });

        describe('when updating roles fails', () => {

          it('calls the proper API endpoints and user sees a Flash notification indicating this error', inject((Flash) => {
            Flash.register(() => {});
            spyOn(Flash, 'error');

            httpBackend
              .expect('PUT', '/api/v0/e/foocorp/users/someone', user)
              .respond(201);
            httpBackend
              .expect('POST', '/api/v0/e/foocorp/authz/users/someone-else', {"set":["observer"]})
              .respond(400, { "error": "bad_request" });
            httpBackend.flush();

            expect(Flash.error).toHaveBeenCalledWith('Error Updating user', undefined);
          }));
        });
      });

      describe('switching a user type', () => {
        beforeEach(() => {
            isolateScope.switchUserType('saml');
        });
        describe('and hitting saveAndClose', () =>{
          it('opens a confirmation modal', () =>{
            spyOn(Modal, 'open');
            expect(isolateScope.user.userType).toEqual('saml');
            expect(isolateScope.currentUserType).toEqual('internal');
            isolateScope.saveAndClose();

            expect(Modal.open).toHaveBeenCalled();
            expect(isolateScope.user.userType).toEqual('saml');
            expect(isolateScope.showEditForm).toEqual(true);
            //don't disable internal user switch back until we hit submit
            expect(isolateScope.internalDisabled).toEqual(false);
          });
        });
        describe('and the cancel button is clicked', () => {
          beforeEach(() => {
              isolateScope.saveAndClose();
          });
          it('hides the confirmation modal', () => {
            expect(isolateScope.user.userType).toEqual('saml');
            spyOn(Modal, 'close');

            isolateScope.closeModal();

            expect(Modal.close).toHaveBeenCalled();
          });

          it('does not save the user', () => {
            spyOn($http, 'put');
            isolateScope.closeModal();
            expect($http.put).not.toHaveBeenCalled();
          });
        });
        describe('and the confirm button is clicked', () =>{
          let user = {
            "first": "Some",
            "last": "One",
            "email": "someone@foo.com",
            "name":"someone",
            "ssh_pub_key": "some-key",
            "user_type" : "saml"
          }
          beforeEach(() => {
              isolateScope.saveAndClose();
          });
          it('calls the put API and user sees a Flash notification', inject((Flash) => {
            expect(isolateScope.user.userType).toEqual('saml');
            Flash.register(() => {});
            spyOn(Flash, 'notify');

            httpBackend
              .expect('PUT', '/api/v0/e/foocorp/users/someone', user)
              .respond(201);
            httpBackend
              .expect('POST', '/api/v0/e/foocorp/authz/users/someone', {"set":["observer"]})
              .respond(201);
            isolateScope.confirmSave();
            isolateScope.closeModal();
            httpBackend.flush();

            expect(Flash.notify).toHaveBeenCalled();
          }));
        });
      });
    });

    describe('editing an existing ldap user', () => {
      beforeEach(() => {
      httpBackend
        .when('GET', '/api/v0/e/foocorp/users/someone')
        .respond({
          "name": "someone",
          "first": "Some",
          "last": "One",
          "email": "someone@foo.com",
          "ssh_pub_key": "some-key",
          "user_type" : "external",
          "_links": {
            "self": {
              "href": "/api/v0/e/foocorp/users/someone"
            },
            "change-password": {
              "href": "/api/v0/e/foocorp/internal-users/someone/change-password"
            }
          }
        });
        httpBackend
          .when('GET', '/api/v0/e/foocorp/authz/users/someone')
          .respond({
            "observer": ["enterprise"]
          });
        httpBackend.expectGET(`/api/v0/e/foocorp/users/someone`);

        isolateScope.toggleEditForm();
        httpBackend.flush();
      });

      it('should not allow switching to other user type', () => {
        expect(isolateScope.showEditForm).toEqual(true);
        expect(isolateScope.user.userType).toEqual('external');
        expect(isolateScope.currentUserType).toEqual('external');

        expect(isolateScope.internalDisabled).toEqual(true);
        expect(isolateScope.samlDisabled).toEqual(false);
        expect(isolateScope.externalDisabled).toEqual(false);
      });

      it('displays role-selection options', () => {
        expect(element[0].querySelectorAll('.roles').length).toBe(1);
      });
    });
    describe('editing an existing saml user', () => {
      beforeEach(() => {
      httpBackend
        .when('GET', '/api/v0/e/foocorp/users/someone')
        .respond({
          "name": "someone",
          "first": "Some",
          "last": "One",
          "email": "someone@foo.com",
          "ssh_pub_key": "some-key",
          "user_type" : "saml",
          "_links": {
            "self": {
              "href": "/api/v0/e/foocorp/users/someone"
            },
            "change-password": {
              "href": "/api/v0/e/foocorp/internal-users/someone/change-password"
            }
          }
        });
        httpBackend
          .when('GET', '/api/v0/e/foocorp/authz/users/someone')
          .respond({
            "observer": ["enterprise"]
          });

        isolateScope.toggleEditForm();
        httpBackend.flush();
      });

      it('should not allow switching to other user type', () => {
        expect(isolateScope.user.userType).toEqual('saml');
        expect(isolateScope.internalDisabled).toEqual(true);
        expect(isolateScope.samlDisabled).toEqual(false);
        expect(isolateScope.externalDisabled).toEqual(false);
      });

      it('displays role-selection options', () => {
        expect(element[0].querySelectorAll('.roles').length).toBe(1);
      });
    });
  });

  describe('as a non-administrator', () => {

    describe('attempting to edit an existing user', () => {

      beforeEach(() => {
        httpBackend
          .when('GET', '/api/v0/e/foocorp/users/someone')
          .respond(403);
      });

      it('displays flash message indicating failure', inject((Flash) => {
        Flash.register(() => {});
        spyOn(Flash, 'error');

        isolateScope.toggleEditForm();
        httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith('Permission denied', 'You must be an Admin to edit users.');
      }));
    });
  });
});
