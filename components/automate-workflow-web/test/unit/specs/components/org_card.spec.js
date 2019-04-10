import ng from 'angular';
import 'angular-mocks';
import OrgCardComponent from '../../../../src/components/org_card/org_card';
import Organizations from '../../../../src/components/organizations/organizations';
import Organization from '../../../../src/common/models/organization';
import Modal from '../../../../src/common/ui/modal/modal';

describe('orgCardComponent', () => {
  let scope, element, isolateScope, $http, $httpBackend, Modal;

  beforeEach(ng.mock.module(Organization));

  beforeEach(ng.mock.module(Organizations, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  function createDirective(Organization, ApiUrl, $compile, _$http_, _$httpBackend_, $rootScope, _Modal_) {
    $http = _$http_;
    $httpBackend = _$httpBackend_;
    Modal = _Modal_;
    scope = $rootScope.$new();

    scope.org = Organization.$buildRaw({ name: 'testOrg' });
    element = $compile(ng.element('<cd-org-card org="org" webhook="webhook"></span>'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }

  beforeEach(inject(createDirective));

  describe('after initialization', () => {
    describe('scope.toggleEditForm()', () => {

      describe('there is no notification config', () => {

        beforeEach(() => {
          $httpBackend
            .when('GET', apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`))
            .respond({});
        });

        it('enabled is set to true on scope.webhook', () => {
          isolateScope.toggleEditForm();
          $httpBackend.flush();
          expect(isolateScope.webhook.enabled).toEqual(true);
        });

        it('scope.toggleEditForm should be false initially', () => {
          expect(isolateScope.showEditForm).toEqual(false);
        });

        it('scope.toggleEditForm() should set showNewOrgForm to true', () => {
          isolateScope.toggleEditForm();
          $httpBackend.flush();
          expect(isolateScope.showEditForm).toEqual(true);
        });

        it('running scope.toggleEditForm() again should set showNewOrgForm to false', () => {
          isolateScope.toggleEditForm();
          $httpBackend.flush();
          isolateScope.toggleEditForm();
          expect(isolateScope.showEditForm).toEqual(false);
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
            .when('GET', apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`))
            .respond(200, response);
          $httpBackend.expectGET(apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`));
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
    })

    describe('scope.cancel()', () => {

        it('resets the slack webhook edit form fields', () => {
          isolateScope.showEditForm = true;
          isolateScope.webhook.name = 'dontwant';
          isolateScope.webhook.url = "https://oops.org"
          isolateScope.cancel();
          expect(isolateScope.webhook.name).toBeUndefined();
          expect(isolateScope.webhook.url).toBeUndefined();
        });

      it('sets scope.showEditForm to false', () => {
        isolateScope.cancel();

        expect(isolateScope.showEditForm).toEqual(false);
      });
    });

    describe('edit org webhook', () => {
      let flash;

      let mocks = {
        success: {
          $promise: {
            then: () => {
              isolateScope.saveAndClose();
              return {
                then: () => {},
                catch: () => {}
              };
            }
          }
        },
        error: {
          $promise: {
            then: () => {
              isolateScope.onError();
              return {
                then: () => {
                  return {
                    catch: () => {}
                  }
                },
                catch: () => {}
              };
            }
          }
        }
      };

      beforeEach(() => {
        isolateScope.showEditForm = true; // a user is editing
      });

      describe('with scope.saveAndClose', () => {

        beforeEach(() => {
          mocks.success =  {
            $promise: {
              then: () => {
                isolateScope.cancel();
                return {
                  then: () => {
                    return {
                      catch: () => {}
                    }
                  },
                  catch: () => {}
                };
              }
            }
          };
        });

        describe('and a webhook has been set', () => {

          let webhook = {
            url: 'http://some-slack-url',
            name: 'Heywood Yapinchme'
          };

          let initialWebhook = {
            url: 'http://some-slack-url',
            name: 'Heywood Yapinchme'
          };

          beforeEach(() => {
            isolateScope.webhook = webhook;
            isolateScope.initialWebhook = initialWebhook;
          });

          describe('and the webhook fields are cleared', () => {

            beforeEach(() => {
              // User cleared out webhook fields.
              isolateScope.webhook.url = '';
              isolateScope.webhook.name = '';
            });

            it('a Modal comes up to make sure you wish to delete the config', () => {
              spyOn(Modal, 'open');

              isolateScope.saveAndClose(isolateScope.webhook);

              expect(Modal.open).toHaveBeenCalled();
              expect(isolateScope.showEditForm).toEqual(true);
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

              it('hides the confirmation modal', () => {
                spyOn(Modal, 'close');

                isolateScope.deleteWebhook();

                expect(Modal.close).toHaveBeenCalled();
              });

              describe('and the user is not an admin', () => {
                var flash;

                beforeEach(inject((Flash) => {
                  $httpBackend
                    .when('DELETE', apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`))
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
                      .when('DELETE', apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`))
                      .respond(204);
                  });

                  it('clears the webhook name, if still set', () => {
                    isolateScope.webhook.name = 'stillhere';
                    isolateScope.deleteWebhook();
                    $httpBackend.flush();
                    expect(isolateScope.webhook.name).toBeUndefined();
                  });

                  it('sets Enabled to true (default)', () => {
                    isolateScope.webhook.enabled = false;
                    isolateScope.deleteWebhook();
                    $httpBackend.flush();
                    expect(isolateScope.webhook.enabled).toBeUndefined();
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
                      .when('DELETE', apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`))
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
        });

        describe('and no webhook has been set', () => {

          let webhook = {
            url: 'http://some-slack-url',
            name: 'Heywood Yapinchme'
          };

          describe('no notification config gets filled in', () => {
            it('closes the edit form and does not make any alerts', () => {
              spyOn(Modal, 'open');

              isolateScope.saveAndClose({});

              expect(Modal.open).not.toHaveBeenCalled();
              expect(isolateScope.showEditForm).toEqual(false);
            });
          });

          describe('scope.createWebhook', () => {

            describe('when successful', () => {
              beforeEach(() => {
                $httpBackend
                  .expectPUT(apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`), webhook)
                  .respond(200);
              });

              it('sets a scope variable indicating success', () => {
                expect(isolateScope.createWebhook(webhook)).toBe(undefined);
                $httpBackend.flush();
              });
            });

            describe('when unsuccessful', () => {

              describe('and the user is not an admin', () => {

                beforeEach(() => {
                  $httpBackend
                    .expectPUT(apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`), webhook)
                    .respond(403);
                });

                it('sets a scope variable indicating failure', inject((Flash) => {
                  Flash.register(() => {});
                  spyOn(Flash, 'error');

                  isolateScope.createWebhook(webhook);
                  $httpBackend.flush();

                  expect(Flash.error).toHaveBeenCalledWith('Forbidden',
                    'You must be an administrator to update an organization.');
                }));
              });

              describe('and the user is an admin', () => {

                beforeEach(() => {
                  $httpBackend
                    .expectPUT(apiUrl(`/orgs/${isolateScope.org.name}/notifications/slack-webhook`), webhook)
                    .respond(412);
                });

                it('sets a scope variable indicating failure', inject((Flash) => {
                  Flash.register(() => {});
                  spyOn(Flash, 'error');

                  isolateScope.createWebhook(webhook);
                  $httpBackend.flush();

                  expect(Flash.error).toHaveBeenCalledWith('Error',
                    'There was a problem saving the Slack webhook.');
                }));
              });
            });
          });

          describe('scope.testWebhook', () => {

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
        });
      });
    });
  });
});
