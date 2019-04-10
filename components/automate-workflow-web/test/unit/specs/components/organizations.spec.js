import ng from 'angular';
import 'angular-mocks';
import Organizations from '../../../../src/components/organizations/organizations';
import Organization from '../../../../src/common/models/organization';

describe('organizationsComponent', () => {
  let scope, element, isolateScope, $httpBackend;

  beforeEach(ng.mock.module(Organization));

  beforeEach(ng.mock.module(Organizations, ($provide) => {
    $provide.value('ApiUrl', apiUrl);
  }));

  function apiUrl(path) {
    return `/api/v0/e/foocorp${path}`;
  }

  function createDirective(Organization, ApiUrl, $compile, _$httpBackend_, $rootScope) {
    $httpBackend = _$httpBackend_;
    scope = $rootScope.$new();

    scope.orgs = Organization.$collection();

    element = $compile(ng.element('<span cd-organizations orgs="orgs"></span>'))(scope);
    isolateScope = element.isolateScope();
    scope.$digest();
  }

  beforeEach(inject(createDirective));

  describe('scope.reverse', () => {

    it('scope.reverse should initialize as false', () => {
      expect(isolateScope.reverse).toEqual(false);
    });

    it('sortReverse(true) should set scope.reverse to true', () => {
      isolateScope.sortReverse(true);
      expect(isolateScope.reverse).toEqual(true);
    });

    it('sortReverse(false) should set scope.reverse to false', () => {
      isolateScope.sortReverse(false);
      expect(isolateScope.reverse).toEqual(false);
    });
  });

  describe('scope.toggleNewOrgForm()', () => {

    it('scope.showNewOrgForm should be false initially', () => {
      expect(isolateScope.showNewOrgForm).toEqual(false);
    });

    it('scope.toggleNewOrgForm() should set showNewOrgForm to true', () => {
      isolateScope.toggleNewOrgForm();
      expect(isolateScope.showNewOrgForm).toEqual(true);
    });

    it('running scope.toggleNewOrgForm() again should set showNewOrgForm to false', () => {
      isolateScope.toggleNewOrgForm();
      isolateScope.toggleNewOrgForm();
      expect(isolateScope.showNewOrgForm).toEqual(false);
    });
  });

  describe('scope.closeForm()', () => {

    it('resets form fields', () => {
      isolateScope.closeForm();

      expect(isolateScope.newOrg).toEqual(null);
      expect(isolateScope.showNewOrgForm).toEqual(false);
      expect(isolateScope.testResult).toBe('');
      expect(isolateScope.webhook.name).toBe(null);
      expect(isolateScope.webhook.url).toBe(null);
      expect(isolateScope.webhook.enabled).toBe(true);
    });
  });

  describe('create new orgs', () => {
    let newOrg = { id: 'newestCoolestOrg' };
    let flash;

    beforeEach(inject((Flash) => {
      Flash.register(() => {});

      spyOn(Flash, 'error');

      flash = Flash;
    }));

    describe('with scope.saveAndClose', () => {

      describe('and it is a valid new org', () => {

        it('sets scope.newOrg to null and scope.showNewOrgForm to false', () => {

          $httpBackend.expectPOST(apiUrl(`/orgs`))
            .respond(201, {
              _links: {
                full: {
                  href: '/api/v0/e/foocorp/orgs/newestCoolestOrg'
                }
              }
            });

          isolateScope.saveAndClose(newOrg);
          $httpBackend.flush();

          expect(isolateScope.newOrg).toEqual(null);
          expect(isolateScope.showNewOrgForm).toEqual(false);
        });

        describe('and the user is not an admin', () => {

          it('shows a Forbidden message', () => {
            $httpBackend.expectPOST(apiUrl(`/orgs`)).respond(403);

            isolateScope.saveAndClose(newOrg);
            $httpBackend.flush();

            expect(flash.error).toHaveBeenCalledWith('Forbidden',
              'You must be an administrator to create an organization.');
          });
        });

        describe('and a webhook has been set', () => {

          let webhook = {
            name: '#some-webhook-name',
            url: 'http://some-slack-url'
          };

          beforeEach(() => {
            isolateScope.webhook = webhook;
          });

          describe('scope.createWebhook', () => {

            describe('when unsuccessful', () => {

              beforeEach(() => {

                var org = {
                  name: 'newestCoolestOrg',
                  links: {
                    full: {
                      href: '/api/v0/e/foocorp/orgs/newestCoolestOrg'
                    }
                  }
                };

                isolateScope.orgs = [org];

                $httpBackend
                  .expectPUT(apiUrl('/orgs/' + org.name + '/notifications/slack-webhook'), webhook)
                  .respond(500);

                isolateScope.createWebhook(webhook);
                $httpBackend.flush();
              });

              it('calls the Flash error component', () => {
                expect(flash.error).toHaveBeenCalled();
              });
            });
          });

          describe('scope.testWebhook', () => {

            describe('when successful', () => {

              beforeEach(() => {
                $httpBackend
                  .expectPUT(apiUrl('/notifications/slack-webhook/test'), {url: webhook.url})
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

      describe('and it is not a valid new org', () => {

        it('calls Flash.error', () => {
          $httpBackend.expectPOST(apiUrl(`/orgs`)).respond(500);

          isolateScope.saveAndClose();
          $httpBackend.flush();

          expect(flash.error).toHaveBeenCalledWith('Error',
            'There was an error saving your new organization.');
        });
      });
    });
  });
});
