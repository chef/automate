import TeamsPage from '../page_objects/teams.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('Teams page', () => {
  let teamsPage, mocks;

  beforeAll(login);
  afterAll(logout);

  beforeEach(() => {

    mocks = [
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/teams$',
          method: 'GET'
        },
        response: {
          body: {
            _links: {},
            teams: ['team1']
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/users$',
          method: 'GET'
        },
        response: {
          body: {
            users: []
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/users/grrm$',
          method: 'GET'
        },
        response: {
          body: {
          }
        }
      }
    ];

    mockApi(mocks);
    browser.get('#/teams');
    teamsPage = new TeamsPage();
  });

  describe('when the feature-flag is enabled', () => {

    it('displays the Teams tab in the left-hand navigation', () => {
      expect(element(by.css('.sidebar li a[ui-sref="main.admin.teams"]'))).not.toBeNull();
    });
  });

  describe('New Team form', () => {

    describe('when user is not an admin', () => {
      beforeEach(() => {
        var authz =
          {
            request: {
              url: '/api/v0/e/Chef/authz/users/grrm$',
              method: 'GET'
            },
            response: {
              status: 403
            }
          };
        mockApi(mocks.concat([authz]));
        browser.get('#/teams');
        teamsPage = new TeamsPage();
      });

      it('disables the new team button', () => {
        expect(teamsPage.newTeamButton.isEnabled()).toBe(false);
      });
    });

    describe('when user is an admin', () => {

      beforeEach(() => {
        var authz =
          {
            request: {
              url: '/api/v0/e/Chef/authz/users/grrm$',
              method: 'GET'
            },
            response: {
              body: {}
            }
          };
        mockApi(mocks.concat([authz]));
        browser.get('#/teams');
        teamsPage = new TeamsPage();
      });

      it('hides the new-team form by default', () => {
        expect(teamsPage.newTeamForm).not.toBePresent();
      });

      it('shows the new-team form', () => {
        teamsPage.newTeamButton.click();
        expect(teamsPage.newTeamForm).toBePresent();
      });

      describe('teams page breadcrumbs', () => {
        it('expects the unclicked team breadcrumb to exist', () => {
          expect(teamsPage.newTeamBreadcrumbNotLinked.isDisplayed()).toBe(true);
        });

        describe('when the new team button is clicked', () => {
          beforeEach(() => {
            teamsPage.newTeamButton.click();
          });

          it('expects the clicked team breadcrumb to exist', () => {
            expect(teamsPage.newTeamBreadcrumbLinked.isDisplayed()).toBe(true);
          });

          it('expects the new team breadcrumb to exist', () => {
            expect(teamsPage.newTeamBreadcrumb.isDisplayed()).toBe(true);
          });
        });
      });

      describe('the new team form', () => {

        beforeEach(() => {
          teamsPage.newTeamButton.click();
        });

        it('save button defaults to disabled', () => {
          expect(teamsPage.createTeamButton.isEnabled()).toBe(false);
        });

        it('save button enables when required fields are filled', () => {
          teamsPage.newTeamName.sendKeys('Water Buffaloes');
          teamsPage.newTeamDescription.sendKeys('Buffalo Ahoy!');
          expect(teamsPage.createTeamButton.isEnabled()).toBe(true);
        });

        describe('when submission succeeds', () => {

          beforeEach(() => {
            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/teams$',
                  method: 'POST',
                  body: {
                    name: 'My Awesome Team',
                    description: 'My team description'
                  }
                },
                response: {
                  status: 201
                }
              }
            ]);
          });

          it('closes the form and redirects to the team page', () => {
            mockApi([
              authorizedLogin,
              {
                request: {
                url: 'api/v0/e/Chef/teams/Water%20Buffaloes',
                method: 'GET'
              },
              response: {
                status: 200
              }
            }]);

            teamsPage.newTeamName.sendKeys('Water Buffaloes');
            teamsPage.newTeamDescription.sendKeys('Buffalo Ahoy!');
            teamsPage.createTeamButton.click();

            expect(teamsPage.newTeamForm.isPresent()).toBe(false);
            expect(browser.getCurrentUrl()).toMatch(/#\/teams\/Water%20Buffaloes/);
          });
        });

        describe('when submission responds with an error', () => {

          beforeEach(() => {
            mockApi([
              authorizedLogin,
              {
                request: {
                  url: '/api/v0/e/Chef/teams$',
                  method: 'POST',
                  body: {
                    name: 'My Awesome Team',
                    description: 'My team description'
                  }
                },
                response: {
                  status: 500
                }
              }
            ]);
          });

          // In UpgradeAdapter/ng12hybrid mode, Protractor may fail to detect the resolution
          // of outstanding promises (for example, with the $interval service) and time out.
          // This instructs Protractor to bypass that default (and usually desired) behavior.
          beforeEach(() => {
            browser.ignoreSynchronization = true;
          });

          afterEach(() => {
            browser.ignoreSynchronization = false;
          });

          it('flashes an error message', () => {
            teamsPage.newTeamName.sendKeys('Water Buffaloes');
            teamsPage.newTeamDescription.sendKeys('Buffalo Ahoy!');
            teamsPage.createTeamButton.click();
            expect(element(by.css('.cd-flash.error'))).toBeDisplayed();
          });
        });
      });

      describe('links to team details', () => {
        it('includes a link to team details page', () => {
          expect(teamsPage.teamEntry('team1').isPresent()).toBe(true);
        });
      });
    });
  });
});
