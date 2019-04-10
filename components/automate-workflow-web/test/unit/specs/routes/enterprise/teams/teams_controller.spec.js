import ng from 'angular';
import 'angular-mocks';
import teamsController
  from '../../../../../../src/routes/enterprise/teams/teams_controller';

describe('teamsController', () => {
  let scope, createController;
  let teams = ['team1', 'team2', 'team3']
  let isAdmin = true

  beforeEach(ng.mock.module(teamsController));

  beforeEach(inject(($controller, $rootScope) => {
    scope = $rootScope.$new();
    createController = () => {
      return $controller('teamsController', {
        $scope: scope,
        teams: teams,
        isAdmin: isAdmin
      });
    };
  }));

  describe('when controller initializes', () => {

    beforeEach(() => {
      createController();
    });

    it('applies a list of teams to the scope', () => {
      expect(scope.teams).toEqual(teams);
    });
  });
});
