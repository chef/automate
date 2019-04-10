import ng from 'angular';
import 'angular-mocks';
import smtpSetupController
  from '../../../../../../src/routes/enterprise/smtp_setup/smtp_setup_controller';

describe('smtpSetupController', () => {
  let scope, createController, smtp;

  beforeEach(ng.mock.module(smtpSetupController));

  beforeEach(inject(($controller, $rootScope, $compile) => {
    scope = $rootScope.$new();

    smtp = {
      authorized: true,
      config: null
    };

    createController = () => {
      return $controller('smtpSetupController', {
        $scope: scope,
        smtp: smtp
      });
    };
  }));

  describe('setup', () => {

    it('applies an SMTP object to the scope', () => {
      createController();
      expect(scope.smtp).toBe(smtp);
    });
  });
});
