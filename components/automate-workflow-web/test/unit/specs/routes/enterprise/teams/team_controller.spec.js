import ng from 'angular';
import 'angular-mocks';
import teamController from '../../../../../../src/routes/enterprise/teams/team_controller';

describe('teamController', () => {
  let scope, createController;
  let team = {
    name: 'team1',
    description: 'my team description',
    creator: 'admin',
    last_modified_at: Date.now()
  }
  let members = [
    {
      username: 'bill',
      fullName: 'Bill Billiams',
      role: 'admin'
    }
  ];
  let users = ['ahab', 'ishmael'];
  let isAdmin = true;

  beforeEach(ng.mock.module(teamController));

  beforeEach(inject(($controller, $rootScope) => {
    scope = $rootScope.$new();
    createController = () => {
      return $controller('teamController', {
        $scope: scope,
        team: team,
        members: members,
        users: users,
        isAdmin: isAdmin
      });
    };
  }));

  describe('when controller initializes', () => {
    beforeEach(() => {
      createController();
    });

    it('gives the team hash to scope', () => {
      expect(scope.team).toEqual(team);
    });

    it('adds the team members list to the scope', () => {
      expect(scope.members).toEqual(members);
    });

    it('adds the users list to the scope', () => {
      expect(scope.users).toEqual(users);
    });

    it('adds isAdmin to the scope', () => {
      expect(scope.isAdmin).toEqual(isAdmin);
    });
  });
});
