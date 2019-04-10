import OrgsPage from '../page_objects/orgs.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('orgs page', () => {

  let orgsPage, mocks;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    orgsPage = new OrgsPage();

    mockApi.clear();
    mocks = [
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/orgs$',
          method: 'GET'
        },
        response: {
          body: {
            orgs: [
              {
                name: 'Alex_Universe',
                project_count: 2
              },
              {
                name: 'Chef_Cookbooks',
                project_count: 1
              },
              {
                name: 'Chef_Delivery',
                project_count: 3
              },
              {
                name: 'Chef_Delivery_Cookbooks',
                project_count: 3
              },
              {
                name: 'sandbox',
                project_count: 4
              }
            ],
            _links: {
              create_org: {
                href: '/api/v0/e/Chef/orgs'
              },
              show_org: {
                href: '/api/v0/e/Chef/orgs/{org_name}',
                templated: true
              }
            }
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
            name: 'grrm',
            first: 'George',
            last: 'Marten',
            email: 'george@getchef.com',
            ssh_pub_key: 'cool_pub_key'
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/authz/users/grrm$',
          method: 'GET'
        },
        response: {
          body: {
            admin: ['enterprise'],
            committer: ['enterprise'],
            reviewer: ['enterprise'],
            shipper: ['enterprise'],
            observer: ['enterprise']
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/users$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {
            users: [
              'a',
              'b',
              'c',
              'd'
            ]
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/pipelines$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: [
            {
              id: 1,
              org: 'fooOrg',
              project: 'fooProject'
            },
            {
              id: 2,
              org: 'barOrg',
              project: 'barProject'
            },
            {
              id: 3,
              org: 'bazOrg',
              project: 'bazProject'
            }
          ]
        }
      },
      {
        request: {
          url: `/bitbucket-servers$`,
          method: 'GET'
        },
        response: {
          body: []
        }
      }
    ];
  });

  describe('creating a new organization', () => {

    beforeEach(() => {
      mockApi(mocks);

      orgsPage.get();
      orgsPage.newOrgButton.click();
    });

    it ('should show the new-org form', () => {
      expect(orgsPage.newOrgForm).toBeDisplayed();
    });

    it ('should display the Slack integration form', () => {
      expect(orgsPage.newWebhookForm).toBeDisplayed();
    });

    describe('and only a webhook URL is provided', () => {

      beforeEach(() => {
        orgsPage.newWebhookUrl.sendKeys('http://some-url');
      });

      it('should indicate name is also required', () => {
        expect(orgsPage.newWebhookNameField).toHaveClass('invalid');
      });

      it('should not be submittable', () => {
        expect(orgsPage.createOrgButton.isEnabled()).toBe(false);
      });
    });

    describe('and both webhook URL and name are provided', () => {

      beforeEach(() => {
        orgsPage.newWebhookName.sendKeys('my-webhook');
        orgsPage.newWebhookUrl.sendKeys('http://some-url');
      });

      describe('and the test button is clicked', () => {

        describe('and the test is successful', () => {

          beforeEach(() => {
            mocks.push({
              request: {
                url: `/api/v0/e/Chef/notifications/slack-webhook/test$`,
                method: 'PUT'
              },
              response: {
                status: 200,
                body: {}
              }
            });
            mockApi(mocks);
          });

          it('should show a success message', () => {
            orgsPage.newWebhookTestButton.click();
            expect(orgsPage.newWebhookTestSuccessMessage).toBePresent();
          });
        });

        describe('and the Slack URL is invalid', () => {

          beforeEach(() => {
            mocks.push({
              request: {
                url: `/api/v0/e/Chef/notifications/slack-webhook/test$`,
                method: 'PUT'
              },
              response: {
                status: 404,
                body: {}
              }
            });
            mockApi(mocks);
          });

          it('should show a failure message', () => {
            orgsPage.newWebhookTestButton.click();
            expect(orgsPage.newWebhookTestErrorMessage).toBePresent();
          });
        });

        describe('and Slack is unreachable', () => {

          beforeEach(() => {
            mocks.push({
              request: {
                url: `/api/v0/e/Chef/notifications/slack-webhook/test$`,
                method: 'PUT'
              },
              response: {
                status: 504,
                body: {}
              }
            });
            mockApi(mocks);
          });

          it('should show a failure message', () => {
            orgsPage.newWebhookTestButton.click();
            expect(orgsPage.newWebhookTestErrorMessage).toBePresent();
          });
        });
      });

      describe('and the submit button is clicked', () => {

        beforeEach(() => {
          orgsPage.newOrgName.sendKeys('best-org-evar');
          orgsPage.createOrgButton.click();

          mocks.push({
            request: {
              url: `/api/v0/e/Chef/orgs$`,
              method: 'POST'
            },
            response: {
              status: 201,
              body: {}
            }
          });
          mockApi(mocks);
        });

        it('should hide the new-org form', () => {
          expect(orgsPage.newOrgForm).not.toBeDisplayed();
        });
      });
    });
  });

  describe('orgs list', () => {

    beforeEach(() => {
      mockApi(mocks);
    });

    it('should display a list of all orgs within the enterprise', () => {
      mockApi(mocks);
      orgsPage.get();

      expect(orgsPage.orgsList.count()).toBe(5);
      orgsPage.orgsList.each((orgsListItem) => {
        expect(orgsListItem).toBeDisplayed();
      });
    });

    describe('filtering', () => {

      it('should filter orgs as you type', () => {
        orgsPage.get();
        orgsPage.filterInput.sendKeys('c');
        orgsPage.filterInput.sendKeys('h');
        orgsPage.filterInput.sendKeys('e');
        orgsPage.filterInput.sendKeys('f');

        expect(orgsPage.orgsListNames)
          .toEqual([
            'Chef_Cookbooks',
            'Chef_Delivery',
            'Chef_Delivery_Cookbooks',
          ]);

        orgsPage.filterInput.clear();
        orgsPage.filterInput.sendKeys('a');
        orgsPage.filterInput.sendKeys('l');
        orgsPage.filterInput.sendKeys('e');
        orgsPage.filterInput.sendKeys('x');

        expect(orgsPage.orgsListNames)
          .toEqual([
            'Alex_Universe'
          ]);
      });
    });

    describe('sorting', () => {

      it('should sort by org name in ascending order by default', () => {
        orgsPage.get();

        // expect(orgsPage.sortAscBtn).toHaveClass('active');

        expect(orgsPage.orgsListNames)
          .toEqual([
            'Alex_Universe',
            'Chef_Cookbooks',
            'Chef_Delivery',
            'Chef_Delivery_Cookbooks',
            'sandbox'
          ]);
      });
    });

    describe('editing', () => {

      describe('when a slack webhook is not configured', () => {

        beforeEach(() => {
          mocks.push({
            request: {
              url: `/notifications/slack-webhook$`,
              method: 'GET'
            },
            response: {
              status: 404,
              body: {
                error: 'not_found'
              }
            }
          });
          mockApi(mocks);
          orgsPage.get();
          orgsPage.editOrgButton.click();
        });

        it('does not display the delete (trashcan) icon', () => {
          expect(orgsPage.notificationTrashcan.isPresent()).toBe(false);
        });
      });

      describe('deleting a configured webhook', () => {
        // deleting a notification by clearing the webhook field
        // has been unit tested in components/org_card.spec.js

        // the behavior of the modal has been unit tested in
        // components/org_card.spec.js as well.

        beforeEach(() => {
          mocks.push({
            request: {
              url: `/notifications/slack-webhook$`,
              method: 'GET'
            },
            response: {
              body: {
                webhook: {
                  url: "https://hooks.slack.com/services/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",
                  name: "#delivery-slack-tests",
                  enabled: true
                }
              }
            }
          });
          mockApi(mocks);
          orgsPage.get();
          orgsPage.editOrgButton.click();
        });

        describe('by clicking the trashcan button', () => {

          it('opens the delete confirmation modal', () => {
            orgsPage.notificationTrashcan.click();
            expect(orgsPage.removeSlackConfirmationModal.isPresent()).toBe(true);
          });
        });
      });
    });
  });

  describe('clicking on an org name', () => {

    beforeEach(() => {
      mockApi([
        authorizedLogin,
        {
          request: {
            url: '/api/v0/e/Chef/orgs$',
            method: 'GET'
          },
          response: {
            body: {
              orgs: [
                {
                  name: 'Alex_Universe',
                  project_count: 2
                },
                {
                  name: 'Chef_Cookbooks',
                  project_count: 1
                },
                {
                  name: 'Chef_Delivery',
                  project_count: 3
                },
                {
                  name: 'Chef_Delivery_Cookbooks',
                  project_count: 3
                },
                {
                  name: 'sandbox',
                  project_count: 4
                }
              ],
              _links: {
                create_org: {
                  href: '/api/v0/e/Chef/orgs'
                },
                show_org: {
                  href: '/api/v0/e/Chef/orgs/{org_name}',
                  templated: true
                }
              }
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/users$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              users: [
                'a',
                'b',
                'c',
                'd'
              ]
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/pipelines$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: [
              {
                id: 1,
                org: 'fooOrg',
                project: 'fooProject'
              },
              {
                id: 2,
                org: 'barOrg',
                project: 'barProject'
              },
              {
                id: 3,
                org: 'bazOrg',
                project: 'bazProject'
              }
            ]
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs/Alex_Universe$',
            method: 'GET'
          },
          response: {
            body: {
              name: 'Alex_Universe',
              project_count: 2,
              _links: {
                self: {
                  href: '/api/v0/e/Chef/orgs/Alex_Universe'
                },
                projects: {
                  href: '/api/v0/e/Chef/orgs/Alex_Universe/projects'
                }
              }
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs/Alex_Universe/projects$',
            method: 'GET'
          },
          response: {
            body: [
              {
                name: 'greentea',
                git_url: 'ssh://git-url',
                scm: {
                  type: 'local'
                }
              },
              {
                name: 'tea',
                git_url: 'ssh://git-url',
                scm: {
                  type: 'local'
                }
              },
              {
                name: 'sweettea',
                git_url: 'ssh://git-url',
                scm: {
                  type: 'local'
                }
              }
            ]
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              name: 'grrm',
              first: 'George',
              last: 'Marten',
              email: 'george@getchef.com',
              ssh_pub_key: 'cool_pub_key'
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/authz/users/grrm$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              admin: ['enterprise'],
              committer: ['enterprise'],
              reviewer: ['enterprise'],
              shipper: ['enterprise'],
              observer: ['enterprise']
            }
          }
        }
      ]);

      orgsPage.get();

      let orgName = orgsPage.orgsList.get(0).$('.card-title a');
      orgName.click();
    });

    it('should navigate to the org page', () => {
      expect(browser.getCurrentUrl())
        .toMatch(/.*\/organizations\/Alex_Universe$/);
    });
  });
});
