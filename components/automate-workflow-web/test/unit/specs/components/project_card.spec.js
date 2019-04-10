import ng from 'angular';
import 'angular-mocks';
import { pick } from 'lodash';
import Project from '../../../../src/common/models/project';
import Organization from '../../../../src/common/models/organization';
import projectCardComponent
from '../../../../src/components/project_card/project_card';
import Modal from '../../../../src/common/ui/modal/modal';


describe('projectCardComponent', () => {
  let scope, isolateScope, element, $httpBackend, $http, Modal;

  beforeEach(ng.mock.module(Organization, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  beforeEach(ng.mock.module(Project));
  beforeEach(ng.mock.module(Organization));
  beforeEach(ng.mock.module(projectCardComponent));

  beforeEach(ng.mock.module(projectCardComponent, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  function createDirective() {
    return inject(($compile) => {
      element = $compile(ng.element(`
        <div
          cd-project-card
          project="project"
          org="org"
          scm-providers="scmProviders">
        </div>`))(scope);
      scope.$digest();
    });
  }
  beforeEach(inject(($rootScope, Organization, ApiUrl, _$httpBackend_, _$http_, _Modal_) => {
    scope = $rootScope.$new();
    $httpBackend = _$httpBackend_;
    Modal = _Modal_;

    scope.org = Organization.$buildRaw({ name: 'FooOrg' });

    scope.project = scope.org.projects.$buildRaw({
      name: 'FooProj',
      scm: {
        type: 'bitbucket'
      }
    });

    scope.scmProviders = [
      {
        type: 'local',
        name: 'Chef Automate',
        scmSetupConfigs: [true]
      },
      {
        type: 'github',
        name: 'Github',
        scmSetupConfigs: []
      },
      {
        type: 'bitbucket',
        name: 'Bitbucket',
        scmSetupConfigs: []
      }
    ];

    $http = _$http_;
    $httpBackend = _$httpBackend_;


    createDirective();
    isolateScope = element.isolateScope();
  }));

  describe('setup', () => {

    it('initializes the edit form fields', () => {
      let expected = pick(scope.project, 'name', 'scm');
      expect(isolateScope.editProject).toEqual(expected);
    });

  });

  describe('edit form', () => {

    it('is hidden by default', () => {
      expect(isolateScope.showEditForm).toBeFalsy();
    });
  });

  describe('toggleEditForm()', () => {
    describe('there is no notification config', () => {

      beforeEach(() => {
        $httpBackend
          .when('GET', apiUrl(`/orgs/${isolateScope.org.name}/projects/${isolateScope.project.name}/notifications/slack-webhook`))
          .respond({});
      });

      it('scope.toggleEditForm() should set showNewOrgForm to true', () => {
        isolateScope.toggleEditForm();
        $httpBackend.flush();
        expect(isolateScope.showEditForm).toEqual(true);
      });

      it('toggles the edit form', () => {
        expect(isolateScope.showEditForm).toEqual(false);
        isolateScope.toggleEditForm();
        expect(isolateScope.showEditForm).toBeTruthy();
        isolateScope.toggleEditForm();
        expect(isolateScope.showEditForm).toBeFalsy();
      });

      it('enabled is set to true on scope.webhook', () => {
        isolateScope.toggleEditForm();
        $httpBackend.flush();
        expect(isolateScope.webhook.enabled).toEqual(true);
      });
    });

    describe('there is notification config', () => {
      let url = 'spooky/beep.boop';
      let name = 'Ghost Robot';
      let enabled = true;

      beforeEach(() => {
        let response = {
          webhook: {
            url: url,
            name: name,
            enabled: enabled
          }
        };

        $httpBackend
          .when('GET', apiUrl(`/orgs/${isolateScope.org.name}/projects/${isolateScope.project.name}/notifications/slack-webhook`))
          .respond(200, response);
        $httpBackend.expectGET(apiUrl(`/orgs/${isolateScope.org.name}/projects/${isolateScope.project.name}/notifications/slack-webhook`));
        isolateScope.toggleEditForm();
        $httpBackend.flush();
      });

      it('scope.webhook.url is set', () => {
        expect(isolateScope.webhook.url).toEqual(url);
      });

      it('scope.webhook.url is set', () => {
        expect(isolateScope.webhook.name).toEqual(name);
      });

      it('scope.webhook.url is set', () => {
        expect(isolateScope.webhook.enabled).toBe(enabled);
      });
    });
  });

  describe('cancel()', () => {

    it('hides the edit form', () => {
      isolateScope.showEditForm = true;
      isolateScope.cancel();
      expect(isolateScope.showEditForm).toBeFalsy();
    });

    it('resets the bitbucket edit form fields', () => {
      isolateScope.showEditForm = true;
      isolateScope.editProject.scm.type = 'local';
      isolateScope.cancel();
      expect(isolateScope.editProject.scm.type).toBe('bitbucket');
    });
  });

  describe('when a Slack webhook has been set up', () => {

    let webhook = {
      name: '#some-webhook-name',
      url: 'http://some-slack-url'
    };

    beforeEach(() => {
      scope.webhook = webhook;
      isolateScope.showEditForm = true;
    });

    describe('cancel()', () => {

      it('resets the slack webhook edit form fields', () => {
        isolateScope.showEditForm = true;
        isolateScope.webhook.name = 'dontwant';
        isolateScope.webhook.url = "https://oops.org"
        isolateScope.cancel();
        expect(isolateScope.webhook.name).toBeUndefined();
        expect(isolateScope.webhook.url).toBeUndefined();
      });
    });

    describe('scope.testWebhook', () => {

      let webhook = {
        url: 'http://some-slack-url',
        name: 'Heywood Yapinchme'
      };

      describe('when successful', () => {

        beforeEach(() => {
          $httpBackend
            .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
            .respond(200);

          isolateScope.testWebhook(webhook);
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
              .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
              .respond(404);

            isolateScope.testWebhook(webhook);
            $httpBackend.flush();
          });

          it('sets a scope variable indicating failure', () => {
            expect(isolateScope.testResult).toMatch('error-any');
          });
        });

        describe('and 302', () => {

          beforeEach(() => {
            $httpBackend
              .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
              .respond(302);

            isolateScope.testWebhook(webhook);
            $httpBackend.flush();
          });

          it('sets a scope variable indicating failure', () => {
            expect(isolateScope.testResult).toMatch('error-any');
          });
        });

        describe('and 504', () => {

          beforeEach(() => {
            $httpBackend
              .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: 'http://some-slack-url'})
              .respond(504);

            isolateScope.testWebhook(webhook);
            $httpBackend.flush();
          });

          it('sets a scope variable indicating failure', () => {
            expect(isolateScope.testResult).toMatch('error-504');
          });
        });
      });
    });

    describe('and the cancel button is clicked', () => {

      it('hides the confirmation modal', () => {
        spyOn(Modal, 'close');

        isolateScope.cancelModal();

        expect(Modal.close).toHaveBeenCalled();
      });

      it('keeps the edit form open', () => {
        isolateScope.cancelModal();
        expect(isolateScope.showEditForm).toEqual(true);
      });

      it('does not call the delete API', () => {
        spyOn($http, 'delete');
        isolateScope.cancelModal();
        expect($http.delete).not.toHaveBeenCalled();
      });
    });

    describe('and the confirm button is clicked', () => {

      describe('and the user is not an admin', () => {
        var flash;

        beforeEach(inject((Flash) => {
          $httpBackend
            .when('DELETE', apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`))
            .respond(403);

          flash = Flash;
          flash.register(() => {});
          spyOn(flash, 'error');
        }));

        it('shows a Forbidden message', () => {
          isolateScope.deleteWebhook();
          $httpBackend.flush();

          expect(flash.error).toHaveBeenCalledWith('Forbidden',
                                                   'You must be an administrator to update an organization.');
        });

        it('does not reset the slack webhook', () => {
          isolateScope.webhook = {
            name: '#my-rfr-channel',
            url: 'https://webhook.slack.com',
            enabled: true
          };
          isolateScope.initialWebhook = {
            name: '#my-rfr-channel',
            url: 'https://webhook.slack.com',
            enabled: true
          };

          isolateScope.deleteWebhook();
          $httpBackend.flush();

          expect(isolateScope.webhook).toEqual({
            name: '#my-rfr-channel',
            url: 'https://webhook.slack.com',
            enabled: true
          });
          expect(isolateScope.initialWebhook).toEqual({
            name: '#my-rfr-channel',
            url: 'https://webhook.slack.com',
            enabled: true
          });
        });

        it('does not close the edit form', () => {
          isolateScope.deleteWebhook();
          $httpBackend.flush();
          expect(isolateScope.showEditForm).toEqual(true);
        });
      });

      describe('and the user is an admin', () => {

        describe('on success', () => {

          beforeEach(() => {
            $httpBackend
              .when('DELETE', apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`))
              .respond(204);
          });

          it('hides the confirmation modal', () => {
            spyOn(Modal, 'close');
            isolateScope.deleteWebhook();
            $httpBackend.flush();
            expect(Modal.close).toHaveBeenCalled();
          });

          it('clears the webhook name, if still set', () => {
            isolateScope.webhook.name = 'stillhere';
            isolateScope.deleteWebhook();
            $httpBackend.flush();
            expect(isolateScope.webhook.name).toBeUndefined();
          });

          it('closes the edit form', () => {
            isolateScope.deleteWebhook();
            $httpBackend.flush();
            expect(isolateScope.showEditForm).toEqual(false);
          });
        });

        describe('on failure', () => {
          var flash;

          beforeEach(inject((Flash) => {
            $httpBackend
              .when('DELETE', apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`))
              .respond(500);

            flash = Flash;
            flash.register(() => {});
            spyOn(flash, 'error');
          }));

          it('shows an Error message', () => {
            isolateScope.deleteWebhook();
            $httpBackend.flush();

            expect(flash.error).toHaveBeenCalledWith('Error',
                                                     'An unexpected error occurred. Failed to delete Slack webhook.');
          });

          it('does not reset the slack webhook', () => {
            isolateScope.webhook = {
              name: '#my-rfr-channel',
              url: 'https://webhook.slack.com',
              enabled: true
            };
            isolateScope.initialWebhook = {
              name: '#my-rfr-channel',
              url: 'https://webhook.slack.com',
              enabled: true
            };

            isolateScope.deleteWebhook();
            $httpBackend.flush();

            expect(isolateScope.webhook).toEqual({
              name: '#my-rfr-channel',
              url: 'https://webhook.slack.com',
              enabled: true
            });
            expect(isolateScope.initialWebhook).toEqual({
              name: '#my-rfr-channel',
              url: 'https://webhook.slack.com',
              enabled: true
            });
          });

          it('does not close the edit form', () => {
            isolateScope.deleteWebhook();
            $httpBackend.flush();
            expect(isolateScope.showEditForm).toEqual(true);
          });
        });
      });
    });
  });

  describe('saveAndClose()', () => {
    beforeEach(() => {
      isolateScope.showEditForm = true;
    });

    describe('when the scm type is github', () => {
      beforeEach(() => {
        isolateScope.editProject.scm.type = 'github';
      });

      it('calls update with githubV2 as the project type', inject((Projects) => {
        let updatedProject = isolateScope.editProject;
        updatedProject.scm.type = 'githubV2'

        $httpBackend
          .expect('PUT', apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}`), updatedProject)
          .respond(201);

        isolateScope.saveAndClose();
        isolateScope.$digest();

        $httpBackend.flush();
      }));

      it('hides the edit form', () => {
        $httpBackend
          .expectPUT( apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}`))
          .respond(201);
        isolateScope.saveAndClose();
        isolateScope.$digest();
        $httpBackend.flush();
        expect(isolateScope.showEditForm).toBeFalsy();
      });

      describe('when a Slack webhook has been set up', () => {

        let webhook = {
          name: '#some-webhook-name',
          url: 'http://some-slack-url'
        };

        let initialWebhook = {
          url: 'http://some-slack-url',
          name: 'Heywood Yapinchme'
        };

        beforeEach(() => {
          isolateScope.webhook = webhook;
          isolateScope.initialWebhook = initialWebhook;
          createDirective();
        });

        it('saves the slack webhook', () => {
          $httpBackend
            .expectPUT( apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}`))
            .respond(201);
          $httpBackend
            .expectPUT(apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`), webhook)
            .respond(201);

          isolateScope.saveAndClose(webhook);
          isolateScope.$digest();
          $httpBackend.flush();
        });

        describe('when the slack webhook save is unsuccessful', () => {
          it('calls the Flash error component', inject((Flash) => {
            $httpBackend
              .expectPUT( apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}`))
              .respond(201);
            $httpBackend
              .expectPUT(apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`), webhook)
              .respond(500);
            spyOn(Flash, 'error');
            isolateScope.saveAndClose(webhook);
            $httpBackend.flush();

            expect(Flash.error).toHaveBeenCalledWith('Error',
                                                     'Your new project was created, but there was a problem saving the Slack webhook.');
          }));
        });

        describe('and the webhook fields are cleared', () => {

          it('a Modal comes up to make sure you wish to delete the config', () => {
            $httpBackend
              .expectPUT( apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}`))
              .respond(201);
            spyOn(Modal, 'open');

            isolateScope.saveAndClose({url: '', name: ''});
            isolateScope.$digest();
            $httpBackend.flush();

            expect(Modal.open).toHaveBeenCalled();
          });
        });
      });
    });

    describe('when save succeeds', () => {

      beforeEach(inject((Projects, $q) => {
        spyOn(Projects, 'update').and.callFake(() => {
          let deferred = $q.defer();
          deferred.resolve();
          return deferred.promise;
        });
      }));

      it('hides the edit form', () => {
        isolateScope.saveAndClose();
        isolateScope.$digest();
        expect(isolateScope.showEditForm).toBeFalsy();
      });

      describe('going to local', () => {
        beforeEach(() => {
          isolateScope.project.scm = {
            type: 'bitbucket',
            projectKey: 'test',
            repoName: 'test'
          };
        });

        it('updates the project', () => {
          isolateScope.editProject.scm.type = 'local';
          isolateScope.saveAndClose();
          isolateScope.$digest();
          expect(isolateScope.project.scm.type).toBe('local');
        });

        it('removes the projectKey and repoName', () => {
          isolateScope.editProject.scm = {
            type: 'local',
            projectKey: 'test',
            repoName: 'test'
          }
          isolateScope.saveAndClose();
          isolateScope.$digest();
          expect(isolateScope.project.scm.projectKey).toBeUndefined();
        });

      });

      describe('when a Slack webhook has been set up', () => {

        let webhook = {
          name: '#some-webhook-name',
          url: 'http://some-slack-url'
        };

        let initialWebhook = {
          url: 'http://some-slack-url',
          name: 'Heywood Yapinchme'
        };

        beforeEach(() => {
          isolateScope.webhook = webhook;
          isolateScope.initialWebhook = initialWebhook;
          createDirective();
        });

        it('saves the slack webhook', () => {
          $httpBackend
            .expectPUT(apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`), webhook)
            .respond(201);

          isolateScope.saveAndClose(webhook);
          isolateScope.$digest();
          $httpBackend.flush();
        });

        describe('when the slack webhook save is unsuccessful', () => {
          beforeEach(() => {
            $httpBackend
              .expectPUT(apiUrl(`/orgs/${scope.org.name}/projects/${scope.project.name}/notifications/slack-webhook`), webhook)
              .respond(500);
          });

          it('calls the Flash error component', inject((Flash) => {
            spyOn(Flash, 'error');
            isolateScope.saveAndClose(webhook);
            $httpBackend.flush();

            expect(Flash.error).toHaveBeenCalledWith('Error',
                                                     'Your new project was created, but there was a problem saving the Slack webhook.');
          }));
        });

        describe('and the webhook fields are cleared', () => {

          it('a Modal comes up to make sure you wish to delete the config', () => {
            spyOn(Modal, 'open');

            isolateScope.saveAndClose({url: '', name: ''});
            isolateScope.$digest();

            expect(Modal.open).toHaveBeenCalled();
          });
        });
      });
    });

    describe('when save fails for an unknown reason', () => {

      beforeEach(inject((Projects, $q) => {
        spyOn(Projects, 'update').and.callFake(() => {
          let deferred = $q.defer();
          deferred.reject({
            data: {
              message:'Project update failed.'
            }
          });
          return deferred.promise;
        });
      }));

      it('displays a generic error message', inject((Flash) => {
        spyOn(Flash, 'error');
        isolateScope.saveAndClose();
        isolateScope.$digest();
        expect(Flash.error)
          .toHaveBeenCalledWith('Error', 'Project update failed.');
      }));
    });

    describe('when save fails because of a database conflict', () => {

      beforeEach(inject((Projects, $q) => {
        spyOn(Projects, 'update').and.callFake(() => {
          let deferred = $q.defer();
          deferred.reject({
            data: {
              message: 'This Bitbucket Project and Repo combination is already in use.'
            }
          });
          return deferred.promise;
        });
      }));

      it('displays an error message telling the user the project and repo are already used', inject((Flash) => {
        spyOn(Flash, 'error');
        isolateScope.saveAndClose();
        isolateScope.$digest();
        expect(Flash.error)
          .toHaveBeenCalledWith('Error', 'This Bitbucket Project and Repo combination is already in use.');
      }));
    });

    describe('when save fails because of open changes', () => {

      beforeEach(inject((Projects, $q) => {
        spyOn(Projects, 'update').and.callFake(() => {
          let deferred = $q.defer();
          deferred.reject({
            data: {
              message: 'You currently have open changes. Please close them and try again.'
            }
          });
          return deferred.promise;
        });
      }));

      it('displays an error message telling the user to close open changes', inject((Flash) => {
        spyOn(Flash, 'error');
        isolateScope.saveAndClose();
        isolateScope.$digest();
        expect(Flash.error)
          .toHaveBeenCalledWith('Error', 'You currently have open changes. Please close them and try again.');
      }));
    });
  });

  describe('toolTipText()', () => {

    it('returns "Bitbucket Project" when the project type is bitbucket', () => {
      expect(isolateScope.scmTypeToolTipText()).toEqual('Bitbucket Project');
    });

    it('returns "GitHub Project" when the project type is github', () => {
      isolateScope.project.scm.type = 'github';
      expect(isolateScope.scmTypeToolTipText()).toEqual('GitHub Project');
    });

    describe('when an external scm link exists', () => {

      it('returns "Go to Bitbucket Repo" when the project type is bitbucket', () => {
        isolateScope.project.scm.type = 'bitbucket';
        isolateScope.project.scm.url = 'https://bitbucket.com/org/repo';
        expect(isolateScope.scmTypeToolTipText()).toEqual('Go to Bitbucket Repo');
      });

      it('returns "Go to GitHub Repo" when the project type is github', () => {
        isolateScope.project.scm.type = 'github';
        isolateScope.project.scm.url = 'https://github.com/org/repo';
        expect(isolateScope.scmTypeToolTipText()).toEqual('Go to GitHub Repo');
      });
    });
  });


  describe('scmIcon()', () => {

    it('returns "fa-bitbucket" when the project type is bitbucket', () => {
      expect(isolateScope.scmIcon()).toEqual('bitbucket');
    });

    it('returns "fa-github" when the project type is github', () => {
      isolateScope.project.scm.type = 'github';
      expect(isolateScope.scmIcon()).toEqual('github');
    });
  });
});
