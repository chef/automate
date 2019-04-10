import ng from 'angular';
import 'angular-mocks';
import smtpSetupComponent from '../../../../src/components/smtp_setup/smtp_setup';
import Modal from '../../../../src/common/ui/modal/modal';

describe('SmtpSetupComponent', () => {
  let scope, element, isolateScope, session, $httpBackend, $http, Modal;

  beforeEach(ng.mock.module(smtpSetupComponent));

  beforeEach(ng.mock.module(smtpSetupComponent, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  function createDirective(smtp) {
    return inject(($compile, $rootScope, _$httpBackend_,  _Modal_, _$http_) => {
      scope = $rootScope.$new();
      $httpBackend = _$httpBackend_;
      Modal = _Modal_;
      $http = _$http_

      scope.smtp = smtp;

      element = $compile(ng.element('<cd-smtp-setup smtp="smtp">'))(scope);
      isolateScope = element.isolateScope();
      scope.$digest();
    });
  };

  describe('when there is no preexisting config', () => {
    beforeEach(() => {
      let smtp = {
        authorized: true,
        config: null,
        user: {
          email: null
        }
       };
       createDirective(smtp);
    });

    describe('on init', () => {

      describe('scope.config', () => {

        it('is assigned no defaults', () => {
          expect(isolateScope.config.host).toBeUndefined();
          expect(isolateScope.config.port).toBeUndefined();
          expect(isolateScope.config.smtp_login).toBeUndefined();
          expect(isolateScope.config.password).toBeUndefined();
          expect(isolateScope.config.sender_email).toBeUndefined();
          expect(isolateScope.config.sender_name).toBeUndefined();
        });
      });
    });

    describe('On save' , () => {

      describe('when unsuccessful', () => {
        let config = {
          host : "beep.boop",
          port : 25,
          smtp_login : "roboty",
          password : "notencrypted",
          sender_email : "beep@boop.com",
          sender_name : "namez"
        };

        beforeEach(() => {
          isolateScope.config = config;
          isolateScope.editPassword();
          $httpBackend
            .expectPUT(apiUrl(`/notifications/smtp`), config)
            .respond(404);
        });

        it('sets a scope variable indicating failure', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'error');

          isolateScope.save();
          $httpBackend.flush();

          expect(Flash.error).toHaveBeenCalled();
        }));
      });

      describe('when optional fields are supplied', () => {
        let config = {
          host : "beep.boop",
          port : 25,
          smtp_login : "roboty",
          password : "notencrypted",
          sender_email : "beep@boop.com",
          sender_name : "namez"
        };

        it('puts config to notifications/smtp', inject((Flash) => {
          Flash.register(() => {});
          spyOn(Flash, 'notify');
          isolateScope.config = config;
          isolateScope.editPassword();

          $httpBackend
            .expectPUT(apiUrl(`/notifications/smtp`), config)
            .respond(201);

          isolateScope.save();

          $httpBackend.flush();
          expect(Flash.notify).toHaveBeenCalled();
        }));
      });
    });
  });

  describe('when there is a preexisting config', () => {

    beforeEach(() => {
      let smtp = {
        authorized: true,
        config: {
          host: 'beep.boop',
          port: 25,
          smtp_login: 'roboty',
          sender_email: 'name@bomb.com',
          sender_name: 'namez'
        },
        user: {
          email: null
        }
      };
      createDirective(smtp);
    });

    it('assigns the config to the ng-models', () => {
      expect(isolateScope.config.host).toEqual('beep.boop');
      expect(isolateScope.config.port).toEqual(25);
      expect(isolateScope.config.smtp_login).toEqual('roboty');
      expect(isolateScope.config.sender_email).toEqual('name@bomb.com');
      expect(isolateScope.config.sender_name).toEqual('namez');
    });


    describe('when password did not change', () => {
      it('puts config to notifications/smtp without password field', inject((Flash) => {
        Flash.register(() => {});
        spyOn(Flash, 'notify');

        let config = {
          host: 'beep.boop',
          port: 25,
          smtp_login: 'roboty',
          sender_email: 'name@bomb.com',
          sender_name: 'namez'
        }

        $httpBackend
          .expectPUT(apiUrl(`/notifications/smtp`), config)
          .respond(201);

        isolateScope.save();

        $httpBackend.flush();
        expect(Flash.notify).toHaveBeenCalled();
      }));
    });

     describe('when password changes', () => {
      let config = {
        host : "beep.boop",
        port : 25,
        smtp_login : "roboty",
        password : "newpassword",
        sender_email : "beep@boop.com",
        sender_name : "namez"
      };

      it('puts config to notifications/smtp with password field', inject((Flash) => {
        Flash.register(() => {});
        spyOn(Flash, 'notify');

        $httpBackend
          .expectPUT(apiUrl(`/notifications/smtp`), config)
          .respond(201);

        isolateScope.config = config;
        isolateScope.editPassword();

        isolateScope.save();

        $httpBackend.flush();
        expect(Flash.notify).toHaveBeenCalled();
      }));
    });


    describe('deleteSmtp()', () => {

      it('a Modal comes up to make sure you wish to delete the config', () => {
        spyOn(Modal, 'open');
        isolateScope.confirmDelete();

        expect(Modal.open).toHaveBeenCalled();
      });

      describe('and the cancel button is clicked', () => {

        it('hides the confirmation modal', () => {
          spyOn(Modal, 'close');

          isolateScope.closeModal();

          expect(Modal.close).toHaveBeenCalled();
        });

        it('does not call the delete API', () => {
          spyOn($http, 'delete');
          isolateScope.closeModal();
          expect($http.delete).not.toHaveBeenCalled();
        });
      });

      describe('and the confirm button is clicked', () => {

        it('hides the confirmation modal', () => {
          spyOn(Modal, 'close');

          isolateScope.deleteSmtp();

          expect(Modal.close).toHaveBeenCalled();
        });

        describe('on success', () =>{
          beforeEach(() => {
            $httpBackend
              .when('DELETE', apiUrl('/notifications/smtp'))
              .respond(204);
            $httpBackend.expectDELETE(apiUrl('/notifications/smtp'));
          });

          it('calls the delete API and user sees a Flash notification', inject((Flash) => {
            Flash.register(() => {});
            spyOn(Flash, 'notify');
            isolateScope.deleteSmtp();
            $httpBackend.flush();

            expect(Flash.notify).toHaveBeenCalled();
          }));
        });

        describe('on failure', () =>{

          it('calls the delete API and user sees a Flash error',  inject((Flash) => {
            Flash.register(() => {});
            spyOn(Flash, 'error');

            $httpBackend
              .when('DELETE', apiUrl('/notifications/smtp'))
              .respond(404);
            $httpBackend.expectDELETE(apiUrl('/notifications/smtp'));

            isolateScope.deleteSmtp();
            $httpBackend.flush();
            expect(Flash.error).toHaveBeenCalled();
          }));
        });
      });
    });
  });

  describe('smtpTest', () => {
    let config = {
      host: 'smtp.google.com',
      port: 25,
      smtp_login: 'helper@smtp.com',
      password: 'ithappeningonenight',
      sender_email: 'helper@ventech.co',
      sender_name: 'helper'
    };

    beforeEach(() => {
      isolateScope.config = config;
    });

    describe('when successful', () => {

      beforeEach(() => {
        $httpBackend
          .expectPOST(apiUrl('/notifications/smtp/test'), config)
          .respond(200);

        isolateScope.editPassword();

        isolateScope.testSmtp();
        $httpBackend.flush();
      });

      it('sets a scope variable indicating success', () => {
        expect(isolateScope.testResult).toBe('success');
      });
    });

    describe('when unsuccessful', () => {

      describe('and 404', () => {

        beforeEach(() => {
          $httpBackend
            .expectPOST(apiUrl('/notifications/smtp/test'), config)
            .respond(404);

          isolateScope.testSmtp();
          $httpBackend.flush();
        });

        it('sets a scope variable indicating failure', () => {
          expect(isolateScope.testResult).toMatch('error');
        });
      });

      describe('and 302', () => {

        beforeEach(() => {
          $httpBackend
            .expectPOST(apiUrl('/notifications/smtp/test'), config)
            .respond(302);

          isolateScope.testSmtp();
          $httpBackend.flush();
        });

        it('sets a scope variable indicating failure', () => {
          expect(isolateScope.testResult).toMatch('error');
        });
      });

      describe('and 504', () => {

        beforeEach(() => {
          $httpBackend
            .expectPOST(apiUrl('/notifications/smtp/test'), config)
            .respond(504);

          isolateScope.testSmtp();
          $httpBackend.flush();
        });

        it('sets a scope variable indicating failure', () => {
          expect(isolateScope.testResult).toMatch('error');
        });
      });
    });
  });
});
