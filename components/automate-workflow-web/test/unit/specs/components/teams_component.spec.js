import ng from 'angular';
import 'angular-mocks';
import teamsComponent from '../../../../src/components/teams/teams';

describe('TeamsComponent', () => {
  let scope, element, httpBackend, teams, TeamsService;

  beforeEach(ng.mock.module(teamsComponent));

  beforeEach(ng.mock.module(teamsComponent, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  beforeEach(inject(($rootScope, $httpBackend, _TeamsService_) => {
    scope = $rootScope.$new();
    httpBackend = $httpBackend;
    TeamsService = _TeamsService_;
  }));

  function createDirective(scope) {
    return inject(($compile) => {
      element = $compile(ng.element('<cd-teams />'))(scope);
      scope.$digest();
    });
  }

  beforeEach(() => {
    teams = ['strawberry', 'blueberry', 'raspberry'];
  });

  describe('initialization', () => {

    beforeEach(() => {
      createDirective(scope);
    });

    it('sets an empty new-team object on the scope', () => {
      expect(scope.team).toEqual({});
    });
  });

  describe('as an administrator', () => {

    beforeEach(() => {
      scope.teams = teams;
      scope.isAdmin = true;
      createDirective(scope);
    });

    describe('new team button', () => {

      it('exists', () => {
        expect(element[0].querySelector('button').title).toBe('New Team');
      });
    });

    describe('scope.toggleNewTeamForm()', () => {

      it('scope.toggleNewTeamForm should be false initially', () => {
        expect(scope.showNewTeamForm).toEqual(false);
      });

      it('scope.toggleNewTeamForm() should set showNewTeamForm to true', () => {
        expect(scope.teams).toEqual(scope.teams);
        scope.toggleNewTeamForm();
        expect(scope.showNewTeamForm).toEqual(true);
      });

      it('running scope.toggleNewTeamForm() again should set showNewTeamForm to false', () => {
        scope.toggleNewTeamForm();
        scope.toggleNewTeamForm();
        expect(scope.showNewTeamForm).toEqual(false);
      });

      it('resets the new-team object', () => {
        scope.team = {
          name: 'The Awesomes',
          description: 'We so are.'
        };
        scope.toggleNewTeamForm();
        expect(scope.team).toEqual({});
      });
    });

    describe('scope.cancel()', () => {

      it('sets scope.showNewTeamForm to false', () => {
        scope.cancel();
        expect(scope.showNewTeamForm).toEqual(false);
      });
    });
  });

  describe('as a non-administrator', () => {

    beforeEach(() => {
      scope.isAdmin = false;
      createDirective(scope);
    });

    describe('new team button', () => {

      it('is disabled', () => {
        var button = element[0].querySelector('button');
        expect(button.title).toBe('New Team');
        expect(button.disabled).toBe(true);
      });
    });
  });

  describe('team list', () => {

    beforeEach(() => {
      scope.teams = teams;
      createDirective(scope);
    });

    it('renders a list-of-teams objects', () => {
      let headings = element[0].querySelector('.cards').querySelectorAll('h2');
      let headingText = Array.from(headings).map((h2) => h2.innerText);
      expect(headingText).toEqual(teams);
    });

    it('renders links for each team card in list', () => {
      let headings = element[0].querySelector('.cards').querySelectorAll('a');
      let headingText = Array.from(headings).map((a) => a.innerText);
      expect(headingText).toEqual(teams);
    });

    describe('scope.submit()', () => {

      beforeEach(() => {
        scope.team = {
          name: 'the-mariners',
          description: 'woo!'
        };
      });

      it('calls TeamsService.create() with the new team name', inject(($q) => {
        spyOn(TeamsService, 'create').and.callFake(() => {
          let deferred = $q.defer();
          deferred.resolve();
          return deferred.promise;
        });
        scope.submit();
        expect(TeamsService.create).toHaveBeenCalledWith(scope.team);
      }));

      describe('when successful', () => {
        let location;

        beforeEach(inject((Flash, $q, $location) => {
          location = $location;
          spyOn(location, 'url');

          spyOn(TeamsService, 'create').and.callFake(() => {
            let deferred = $q.defer();
            deferred.resolve();
            return deferred.promise;
          });
        }));

        it('calls $location.url() with the new team name', () => {
          scope.submit();
          scope.$digest();
          expect(location.url).toHaveBeenCalledWith(`/teams/${scope.team.name}`);
        });
      });

      describe('when unsuccessful', () => {
        let flash;

        beforeEach(inject((Flash, $q) => {
          Flash.register(() => {});
          spyOn(Flash, 'error');
          flash = Flash;

          spyOn(TeamsService, 'create').and.callFake(() => {
            let deferred = $q.defer();
            deferred.reject();
            return deferred.promise;
          });
        }));

        it('calls Flash.error() with the appropriate message', () => {
          scope.submit();
          scope.$digest();
          expect(flash.error).toHaveBeenCalledWith('Error adding team', 'There was a problem adding your team. Please try again.');
        });
      });
    });
  });
});
