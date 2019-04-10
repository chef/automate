import ng from 'angular';
import 'angular-mocks';
import samlSetupController
  from '../../../../../../src/routes/enterprise/saml_setup/saml_setup_controller';

describe('samlSetupController', () => {
  let scope, createController, saml;

  beforeEach(ng.mock.module(samlSetupController));

  beforeEach(inject(($controller, $rootScope, $compile) => {
    scope = $rootScope.$new();

    saml = {
      authorized: true,
      config: null
    };

    createController = () => {
      return $controller('samlSetupController', {
        $scope: scope,
        saml: saml
      });
    };
  }));

  describe('setup', () => {

    it('applies a SAML object to the scope', () => {
      createController();
      expect(scope.saml).toBe(saml);
    });
  });
});
