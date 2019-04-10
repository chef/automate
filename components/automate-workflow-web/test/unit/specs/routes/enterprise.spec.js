import ng from 'angular';
import 'angular-mocks';
import enterpriseController
  from '../../../../src/routes/enterprise/enterprise_controller';

describe('enterpriseController', () => {
  let scope, createController, orgs;

  beforeEach(ng.mock.module(enterpriseController));

  beforeEach(inject(($controller, $rootScope, $compile) => {
    scope = $rootScope.$new();
    orgs = [{ name: 'ponyville' }];

    createController = () => {
      return $controller('enterpriseController', {
        $scope: scope,
        orgs: orgs
      });
    };
  }));

  describe('setup', () => {

    it('attaches orgs to the scope', () => {
      createController();
      expect(scope.orgs).toBe(orgs);
    });
  });
});
