import TeamPage from '../page_objects/team.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('Team page', () => {
  let teamPage, mocks;

  beforeAll(login);
  afterAll(logout);

  beforeEach(() => {
    mocks = [
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/users$',
          method: 'GET'
        },
        response: {
          body: {
            users: ['alice', 'bob', 'cathy', 'don', 'elizabeth', 'frank', 'gabrielle', 'homer', 'irene',
                    'jose', 'kate', 'leon', 'marisol', 'adam', 'bobby', 'cain', 'denise', 'ellis', 'farah',
                    'george', 'hermione', 'indigo', 'janet', 'keith', 'lara', 'michael', 'alanis', 'alicia',
                    'aaron', 'aramis']
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
      },
      {
        request: {
          url: '/api/v0/e/Chef/teams/team1$',
          method: 'GET'
        },
        response: {
          body: {
            name: 'team1',
            description: 'This is my test team and it\'s awesome. This description might get ' +
              'a little long but that\'s okay. The description text should probably wrap after ' +
              'getting half-way across the screen. Multiple lines should be supported, probably ' +
              'up to 255 characters.',
            creator_name: 'creator',
            updater_name: 'updater',
            updated_at: '2016-09-15 15:47:22'
          }
        }
      }
    ];

    teamPage = new TeamPage();
  });

  describe('team detail section', () => {

    beforeEach(() => {
      mockApi(mocks);
      browser.get('#/teams/team1');
    });

    it('has the team name', () => {
      expect(teamPage.teamTitle.getText()).toEqual('team1');
    });

    it('includes team description', () => {
      let text = 'This is my test team and it\'s awesome. This description might get ' +
        'a little long but that\'s okay. The description text should probably wrap after ' +
        'getting half-way across the screen. Multiple lines should be supported, probably ' +
        'up to 255 characters.';
      expect(teamPage.teamDescription.getText()).toEqual(text);
    });

    it('has the creator username', () => {
      expect(teamPage.teamCreator.getText()).toEqual('Created by: creator');
    });

    it('has the team details', () => {
      expect(teamPage.teamModified.getText()).toEqual('Modified September 15, 2016 3:47 PM by updater');
    });
  });

  describe('when there are members of the team', () => {

    beforeEach(() => {
      mockApi(mocks.concat([{
        request: {
          url: '/api/v0/e/Chef/teams/team1/members$',
          method: 'GET'
        },
        response: {
          body: {
            members: [
              {
                name: 'Bill',
                full_name: 'Bill Billiams',
                role: 'admin'
              }
            ]
          }
        }
      }]));
      browser.get('#/teams/team1');
    });

    describe('members table', () => {
      it('has the team member username', () => {
        expect(teamPage.members.first().element(by.css('td.username')).getText()).toEqual('Bill');
      });

      it('has the team member full name', () => {
        expect(teamPage.members.first().element(by.css('td.full-name')).getText()).toEqual('Bill Billiams');
      });

      it('has the team member role', () => {
        expect(teamPage.members.first().element(by.css('td.role')).getText()).toEqual('admin');
      });
    });
  });

  describe('when there are no members of the team', () => {

    beforeEach(() => {
      mockApi(mocks.concat([{
        request: {
          url: '/api/v0/e/Chef/teams/team1/members$',
          method: 'GET'
        },
        response: {
          body: {
            members: []
          }
        }
      }]));
      browser.get('#/teams/team1');
    });

    it('shows a message indicating the team has no members', () => {
      let element = teamPage.noTeamMembersMessage;
      expect(element.isDisplayed()).toBe(true);
      expect(element.getText()).toEqual('No one has been added to this team yet.');
    });
  });

  describe('new team member form', () => {
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
        browser.get('#/teams/team1');
      });

      it('disables the new member text field and form', () => {
        expect(teamPage.addMemberInput.isEnabled()).toBe(false);
        expect(teamPage.addMemberForm.getAttribute('class')).toMatch('disabled-for-non-admin');
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
        browser.get('#/teams/team1');
      });

      it('the new team member form is enabled', () => {
        expect(teamPage.addMemberInput.isEnabled()).toBe(true);
        expect(teamPage.addMemberForm.getAttribute('class')).not.toMatch('disabled-for-non-admin');
      });

      it('add team member button defaults to disabled', () => {
        expect(teamPage.addMemberButton.isEnabled()).toBe(false);
      });

      it('when the search term has multiple results', () => {
        teamPage.addMemberInput.sendKeys('a');
        expect(teamPage.addMemberResultsDropdown.isDisplayed()).toBe(true);
        expect(teamPage.addMemberButton.isEnabled()).toBe(false);
      });

      it('when the search term has a single valid result', () => {
        teamPage.addMemberInput.sendKeys('adam');
        expect(teamPage.addMemberResultsDropdown.isDisplayed()).toBe(true);
        expect(teamPage.addMemberButton.isEnabled()).toBe(true);
      });

      it('when the search term has no results', () => {
        teamPage.addMemberInput.sendKeys('no_such_user');
        expect(teamPage.addMemberResultsDropdown.isDisplayed()).toBe(false);
        expect(teamPage.addMemberButton.isEnabled()).toBe(false);
      });

      describe('when a member is submitted successfully', () => {
        beforeEach(() => {
          mockApi([
            authorizedLogin,
            {
              request: {
                url: '/api/v0/e/Chef/teams/team1/members/adam$',
                method: 'PUT',
                body: {}
              },
              response: {
                status: 201
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/teams/team1/members$',
                method: 'GET'
              },
              response: {
                body: {
                  members: [
                    {
                      name: 'adam',
                      full_name: 'Adam West',
                      role: 'admin'
                    }
                  ]
                }
              }
            }
          ]);
        });

        // Marked pending because of occasional failures
        xit('posts the data to the server', () => {
          expect(teamPage.members.count()).toBe(0);

          teamPage.addMemberInput.sendKeys('adam');
          expect(teamPage.usersInDropdown.count()).toBe(1);

          teamPage.usersInDropdown.first().click();
          teamPage.addMemberButton.click();
          expect(teamPage.members.count()).toBe(1);
        });
      });
    });
  });
});
