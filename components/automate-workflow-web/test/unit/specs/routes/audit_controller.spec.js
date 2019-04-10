import ng from 'angular';
import 'angular-mocks';
import auditController
  from '../../../../src/routes/audit/audit_controller';

describe('auditController', () => {
  let scope, createController, auditLog;

  beforeEach(ng.mock.module(auditController));

  beforeEach(inject(($controller, $rootScope, $compile) => {
    scope = $rootScope.$new();
    auditLog = [
      {
        title: "THIS IS A FAKE CHANGE",
        delivered_by: "testUser1",
        delivered_at: "2015-08-28 22:17:45",
        org: "testing_organization",
        project: "delivery",
        change_id: "11111"
      },
      {
        title: "Just ship it already",
        delivered_by: "testUser2",
        delivered_at: "2015-08-28 22:17:45",
        org: "testing_organization",
        project: "delivery",
        change_id: "22222"
      },
      {
        title: "Another cool change",
        delivered_by: "testUser3",
        delivered_at: "2015-08-23 22:17:45",
        org: "testing_organization",
        project: "delivery",
        change_id: "33333"
      }
    ]
    createController = () => {
      return $controller('auditController', {
        $scope: scope,
        auditLog: auditLog
      });
    };
  }));

  describe('setup', () => {

    it('should attach the historical changes to the scope', () => {
      createController();
      expect(scope.auditLog).toEqual(auditLog);
    });
  });
});
