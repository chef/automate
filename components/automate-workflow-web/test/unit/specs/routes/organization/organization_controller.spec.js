import ng from 'angular';
import 'angular-mocks';
import organizationController
  from '../../../../../src/routes/organization/organization_controller';
import Organization from '../../../../../src/common/models/organization';
import Project from '../../../../../src/common/models/project';

describe('organizationController', () => {
  let scope, createController, scmProviders, newProject, Flash, $httpBackend,
      organization, projects, addproject, $state, featureFlags;

  beforeEach(ng.mock.module(Organization));
  beforeEach(ng.mock.module(organizationController, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  beforeEach(inject(($controller, $rootScope, $compile, _$httpBackend_,
                     Organization, ApiUrl, Project, _$state_) => {
    scope = $rootScope.$new();
    $httpBackend = _$httpBackend_;
    $state = _$state_;

    Flash = jasmine.createSpyObj('Flash', ['notify', 'error']);
    featureFlags = jasmine.createSpyObj('featureFlags', ['isOn']);


    scmProviders = [
      {
        name: 'GitHub',
        type: 'github',
        setupUrl: '/github-servers'
      },
      {
        name: 'Bitbucket',
        type: 'bitbucket',
        setupUrl: '/bitbucket-servers'
      }
    ];

    newProject = {
      name: 'FooProj',
      scm: {
        type: 'local',
        projectCreateUri: '/projects'
      }
    };

    organization = Organization.$buildRaw({
      name:'my-org',
      links: {
        full: {
          href: '/api/v0/e/foocorp/orgs/my-org'
        }
      }
    });

    createController = () => {
      return $controller('organizationController', {
        $scope: scope,
        Flash: Flash,
        scmProviders: scmProviders,
        newProject: newProject,
        organization: organization,
        projects: Project.$collection(),
        addproject: false,
        featureFlags: featureFlags
      });
    };
  }));

  describe('saveAndClose()', () => {
    beforeEach(() => {
      createController();
      scope.scmSetupForm = { '$valid': true };
    });

    describe('when the server responds with a failure code', () => {
      it('should show the error message', () => {
        $httpBackend
          .expectPOST(newProject.scm.projectCreateUri)
          .respond((method, url, data, headers, params) => {
            return [409, {'message':"The repository name you entered has already been linked to another project in Delivery."}];
          });
        scope.saveAndClose(newProject);

        $httpBackend.flush();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          "The repository name you entered has already been linked to another project in Delivery."
        );
      });
    });

    describe('when slack integration is enabled and a webhook has been set up', () => {

      let webhook = {
        name: '#some-webhook-name',
        url: 'http://some-slack-url'
      };

      beforeEach(() => {
        scope.webhook = webhook;

        createController();

        $httpBackend
          .expectPOST(newProject.scm.projectCreateUri)
          .respond(201);
        spyOn(scope.projects, '$refresh').and.returnValue({});
        spyOn($state, 'go').and.returnValue({});
      });

      it('saves the slack webhook', () => {
        $httpBackend
          .expectPUT(apiUrl(`/orgs/${organization.name}/projects/${newProject.name}/notifications/slack-webhook`), webhook)
          .respond(201);

        scope.saveAndClose(newProject, webhook);
        $httpBackend.flush();
      });

      describe('when the slack webhook save is unsuccessful', () => {
        beforeEach(() => {
          $httpBackend
            .expectPUT(apiUrl(`/orgs/${organization.name}/projects/${newProject.name}/notifications/slack-webhook`), webhook)
            .respond(500);
        });

        it('calls the Flash error component', () => {
          scope.saveAndClose(newProject, webhook);
          $httpBackend.flush();

          expect(Flash.error).toHaveBeenCalledWith('Error',
            'Your new project was created, but there was a problem saving the Slack webhook.');
        });
      });
    });
  });

  describe('when slack integration is enabled', () => {

    beforeEach(() => {
      createController();
      scope.scmSetupForm = { '$valid': true };
    });

    describe('and a webhook has been set up', () => {

      let webhook = {
        name: '#some-webhook-name',
        url: 'http://some-slack-url'
      };

      beforeEach(() => {
        scope.webhook = webhook;
      });

      describe('scope.testWebhook', () => {

        describe('on success', () => {

          beforeEach(() => {
            $httpBackend
              .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: webhook.url})
              .respond(200);

            scope.testWebhook(webhook);
            $httpBackend.flush();
          });

          it('sets a scope variable indicating success', () => {
            expect(scope.testResult).toBe('success');
          });
        });

        describe('on failure', () => {

          describe('and the response is 302', () => {

            beforeEach(() => {
              $httpBackend
                .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
                .respond(302);

              scope.testWebhook(webhook);
              $httpBackend.flush();
            });

            it('sets a scope variable indicating failure', () => {
              expect(scope.testResult).toMatch('error-any');
            });
          });

          describe('and the response is 404', () => {

            beforeEach(() => {
              $httpBackend
                .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
                .respond(404);

              scope.testWebhook(webhook);
              $httpBackend.flush();
            });

            it('sets a scope variable indicating failure', () => {
              expect(scope.testResult).toMatch('error-any');
            });
          });

          describe('and the response is 504', () => {

            beforeEach(() => {
              $httpBackend
                .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
                .respond(504);

              scope.testWebhook(webhook);
              $httpBackend.flush();
            });

            it('sets a scope variable indicating failure', () => {
              expect(scope.testResult).toMatch('error-504');
            });
          });
        });
      });
    });
  });
});
