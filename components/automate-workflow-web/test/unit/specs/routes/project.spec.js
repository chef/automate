import ng from 'angular';
import 'angular-mocks';
import projectController from '../../../../src/routes/project/project_controller';

describe('projectController', () => {
  let scope, createController, location, organization, project,
      dependencies, watchSettings, $httpBackend, sessionObj;

  beforeEach(ng.mock.module(projectController, ($provide) => {
    $provide.value('Session', {
      get: () => sessionObj,
    });
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/workflow/api/v0/e/foocorp/${path}`;
  }

  beforeEach(ng.mock.module(projectController, ($provide) => {
    $provide.decorator('$location', ($delegate) => {
      let state = {};

      $delegate.search = (param, val) => {
        if (param && val) {
          return state[param] = val;
        }
        return state;
      };

      return $delegate;
    });
  }));

  beforeEach(inject(($controller, $rootScope, _$httpBackend_) => {
    scope = $rootScope.$new();
    $httpBackend = _$httpBackend_;
    sessionObj = { username: 'applejack' };
    organization = { name: 'fooOrg' };

    project = {
      name: 'fooProject',
      changes: [{ id: 1 }],
      pipelines: [{ name: 'master' }]
    };

    dependencies = {
      dependencies: [],
      required_by: []
    };

    watchSettings = {
      watchable: false,
      categories: []
    };

    createController = (opts = {}) => {
      return $controller('projectController', {
        $scope: scope,
        organization: organization,
        project: project,
        changes: project.changes,
        dependencies: dependencies,
        watchSettings: opts.watchSettings || watchSettings
      });
    };
  }));

  describe('setup', () => {

    beforeEach(() => {
      createController();
    });

    it('attaches the project, org, changes and watch settings to the scope', () => {
      expect(scope.project.name).toBe(project.name);
      expect(scope.changes).toBe(project.changes);
      expect(scope.project.pipelines).toBe(project.pipelines);
      expect(scope.watchSettings).toBe(watchSettings);
    });

    it('scope.watchSettings.watching initializes as false', () => {
      expect(scope.watchSettings).toBe(watchSettings);
    });

    it('sets scope.watchableCategories', () => {
      expect(scope.watchableCategories).toEqual([
        {
          name: 'review',
          description: 'Verify stage passed, change approved, comment added to change.'
        },
        {
          name: 'deliver',
          description: 'Acceptance stage passed, change delivered.'
        },
        {
          name: 'observe',
          description: 'Any failures in Build, Acceptance, Union, Rehearsal, and Delivered stages. Successful completion of Delivered stage.'
        }
      ]);
    });
  });

  describe('watchSettings', () => {

    describe('when SMTP has been configured', () => {

      beforeEach(() => {
        watchSettings.watchable = true;
      });

      describe('and the user is watching the project', () => {

        beforeEach(() => {
          watchSettings.categories = [
            'review'
          ];

          createController({
            watchSettings: watchSettings
          });
        });

        it ('applies the right watch-button settings', () => {
          expect(scope.watchButton.icon).toBe('eyeball');
          expect(scope.watchButton.title).toBe('Change your watch settings for this project');
          expect(scope.watchButton.disabled).toBe(false);
        });
      });

      describe('and the user is not watching the project', () => {

        beforeEach(() => {
          createController({
            watchSettings: watchSettings
          });
        });

        it ('applies the right watch-button settings', () => {
          expect(scope.watchButton.icon).toBe('eyeball-off');
          expect(scope.watchButton.label).toBe('Watch Project');
          expect(scope.watchButton.title).toBe('Watch this project to be notified of changes by email');
          expect(scope.watchButton.disabled).toBe(false);
        });
      });
    });

    describe('when SMTP has not been configured', () => {

      beforeEach(() => {
        watchSettings.watchable = false;

        createController({
          watchSettings: watchSettings
        });
      });

      it ('disables the watch button', () => {
        expect(scope.watchButton.icon).toBe('eyeball-off');
        expect(scope.watchButton.label).toBe('Watch Project');
        expect(scope.watchButton.title).toBe('An admin has not yet configured email for your enterprise');
        expect(scope.watchButton.disabled).toBe(true);
      });
    });
  });

  describe('scope.toggleWatchMenu', () => {

    beforeEach(() => {
      createController();
    });

    it('toggles showWatchMenu', () => {
      expect(scope.showWatchMenu).toBe(false);
      scope.toggleWatchMenu();
      expect(scope.showWatchMenu).toBe(true);
    });
  });

  describe('scope.toggleCategory', () => {

    beforeEach(() => {
      createController({ watchSettings: {
        categories: ['observe']
      }});
    });

    describe('when the category is not selected', () => {

      it('sends a put with the selected category', () => {
        $httpBackend.expectPUT(apiUrl(`/orgs/${scope.organization.name}/projects/${scope.project.name}/notifications/watch`), {
          categories: ['observe', 'review']
        }).respond(200);

        scope.toggleCategory('review');

        $httpBackend.flush();

        expect(scope.watchSettings.categories.indexOf('review')).toEqual(1);
      });
    });

    describe('when the category is selected', () => {

      it ('sends a PUT without the category', () => {
        scope.watchSettings.categories.push('review');

        $httpBackend.expectPUT(apiUrl(`/orgs/${scope.organization.name}/projects/${scope.project.name}/notifications/watch`), {
          categories: ['observe']
        }).respond(200);

        scope.toggleCategory('review');

        $httpBackend.flush();
      });
    });
  });

  describe('filtering by merge status', () => {

    it('should be off by default', () => {
      createController();

      expect(scope.currentFilter.text).toBe('All');
      expect(scope.currentFilter.value).toBe('all');
    });

    it('updates the filter state param', inject(($location) => {
      createController();

      scope.$digest();
      scope.filterByState({ value: 'merged' });
      scope.$digest();
      expect($location.search().state).toBe('merged');
    }));
  });

  describe('clonesFromDelivery()', () => {

    it('returns true for "local" and "bitbucket" and "githubV2"', () => {
      createController();
      expect(scope.clonesFromDelivery('local')).toBe(true);
      expect(scope.clonesFromDelivery('bitbucket')).toBe(true);
      expect(scope.clonesFromDelivery('githubV2')).toBe(true);
    });

    it('returns false for "github"', () => {
      createController();
      expect(scope.clonesFromDelivery('github')).toBe(false);
    });
  });
});
