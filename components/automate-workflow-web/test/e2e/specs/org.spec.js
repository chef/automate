import OrgPage from '../page_objects/org.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('org page', () => {

  let orgPage, mocks;

  let params = {
    forNewOrg: {
      org: 'a_new_org'
    },
    forExistingOrg: {
      org: 'an_existing_org'
    }
  };

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    orgPage = new OrgPage();

    mockApi.clear();

    mocks = [
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/orgs$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {
            orgs: []
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/a_new_org$',
          method: 'GET'
        },
        response: {
          body: {
            name: 'a_new_org',
            project_count: 0
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/a_new_org/projects$',
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/an_existing_org$',
          method: 'GET'
        },
        response: {
          body: {
            name: 'an_existing_org',
            project_count: 2

          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/an_existing_org/projects$',
          method: 'GET'
        },
        response: {
          // NOTE: in the tests below, we call .first() and .last() on the list
          // of projects as they are displayed in the UI. When they are displayed
          // in the UI, they are sorted in alphabetical order, by name. To aid
          // future developers, please put new projects for this list in sorted
          // alphabetical order.
          body: [
            {
              name: 'bitbucket_project',
              git_url: 'ssh://git-url',
              scm: {
                type: 'bitbucket',
                url: 'https://bitbucket.com/org/project'
              }
            },
            {
              name: 'github_project',
              git_url: 'ssh://git-url',
              scm: {
                type: 'github'
              }
            },
            {
              name: 'githubv2_project',
              git_url: 'ssh://git-url',
              scm: {
                type: 'githubV2',
                url: 'https://github.com/owner/project'
              }
            },
            {
              name: 'my_first_project',
              git_url: 'ssh://git-url',
              scm: {
                type: 'local'
              }
            },
            {
              name: 'my_second_project',
              git_url: 'ssh://git-url',
              scm: {
                type: 'local'
              }
            }
          ]
        }
      }
    ];
  });

  describe('the project list', () => {
    let projectCard;

    beforeEach(() => {
      mockApi(mocks);
      orgPage.get(params.forExistingOrg);
    });

    describe('a local project', () => {
      beforeEach(() => {
        projectCard = orgPage.projectCards.last();
      });

      it('has no scm icon', () => {
        let icon = projectCard.element(by.css('.scm-icon'));
        expect(icon).not.toBePresent();
      });

      it('has no link to the external scm', () => {
        let link = projectCard.element(by.css('a.scm-link'));
        expect(link).not.toBePresent();
      });
    });

    describe('a bitbucket-backed project', () => {
      beforeEach(() => {
        projectCard = orgPage.projectCards.first();
      });

      it('has a bucket icon', () => {
        let icon = projectCard.element(by.css('.scm-icon.fa-bitbucket'));
        expect(icon).toBePresent();
      });

      it('has a link to the external scm', () => {
        let link = projectCard.element(by.css('a.scm-link'));
        expect(link).toBePresent();
      });
    });

    describe('a github-backed project', () => {
      beforeEach(() => {
        projectCard = orgPage.projectCards.get(2);
      });

      it('has a github kitty icon', () => {
        let icon = projectCard.element(by.css('.scm-icon.fa-github'));
        expect(icon).toBePresent();
      });

      it('has a link to the external scm', () => {
        let link = projectCard.element(by.css('a.scm-link'));
        expect(link).toBePresent();
      });
    });

    describe('a github-backed project without a link', () => {
      beforeEach(() => {
        projectCard = orgPage.projectCards.get(1);
      });

      it('has a github kitty icon', () => {
        let icon = projectCard.element(by.css('.scm-icon.fa-github'));
        expect(icon).toBePresent();
      });

      it('has no link to the external scm', () => {
        let link = projectCard.element(by.css('a.scm-link'));
        expect(link).not.toBePresent();
      });

    });

  });

  describe('when projects exist without top-level bitbucket SCM link', () => {

    beforeEach(() => {

      mockApi(mocks);
      mockApi([{
        request: {
          url: `/scm-providers$`,
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: [true]
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: []
            }
          ]
        }
      }]);

      orgPage.get(params.forExistingOrg);
    });

    it('hides the new-project form', () => {
      expect(orgPage.newProjectForm.isPresent()).toBe(false);
    });

    it('shows the list of existing projects', () => {
      expect(orgPage.projectCards.count()).toEqual(5);
    });

    describe('project list item', () => {
      let editBtn, projectItem;

      beforeEach(() => {
        projectItem = orgPage.projectCards.last();
        editBtn = projectItem.element(by.css('.edit-project'));
      });

      it('has an edit button', () => {
        expect(editBtn).toBeDisplayed();
      });

      describe('clicking the edit button', () => {

        it('toggles the edit form', () => {
          editBtn.click();

          let editForm = projectItem.element(by.css('.edit-project-form'));
          expect(editForm).toBeDisplayed();

          editBtn.click();
          expect(editForm).not.toBePresent();
        });
      });

      describe('editing a project', () => {
        let editForm;

        beforeEach(() => {
          editBtn.click();
          editForm = projectItem.element(by.css('.edit-project-form'));
        });

        describe('changing SCM providers', () => {

          describe('from local project', () => {

            describe('local -> bitbucket', () => {

              beforeEach(() => {
                let bitbucketOption = editForm.element(by.cssContainingText('a', 'Bitbucket'));
                bitbucketOption.click();
              });

              it('displays an error message', () => {
                let errorMsg = editForm.element(by.css('.project-scm-instructions'));
                expect(errorMsg).toBeDisplayed();
              });
            });

            describe('local -> github', () => {

              beforeEach(() => {
                let githubOption = editForm.element(by.cssContainingText('a', 'GitHub'));
                githubOption.click();
              });

              it("doesn't display an error message", () => {
                let errorMsg = editForm.element(by.css('.project-scm-instructions'));
                expect(errorMsg).not.toBePresent();
              });
            });
          });

          describe('from bitbucket project', () => {
            let editBtn, editForm, bitbucketProject;

            beforeEach(() => {
              bitbucketProject = orgPage.projectCards.first();
              editBtn = bitbucketProject.element(by.css('.edit-project'));
              editBtn.click();
              editForm = bitbucketProject.element(by.css('.edit-project-form'));
            });

            describe('bitbucket -> github', () => {

              beforeEach(() => {
                let githubOption = editForm.element(by.cssContainingText('a', 'GitHub'));
                githubOption.click();
              });

              it('displays an error message', () => {
                let errorMsg = editForm.element(by.css('.project-scm-instructions'));
                expect(errorMsg).toBeDisplayed();
              });
            });

            describe('bitbucket -> local', () => {

              beforeEach(() => {
                let localOption = editForm.element(by.cssContainingText('a', 'Chef Automate'));
                localOption.click();
              });

              it('enables the save button', () => {
                let saveBtn = editForm.element(by.buttonText('Save & Close'));
                expect(saveBtn.isEnabled()).toBe(true);
              });
            });
          });

        });

        describe('submitting edit form', () => {

          beforeEach(() => {
            mockApi([{
              request: {
                url: '/api/v0/e/Chef/orgs/an_existing_org/projects/my_second_project$',
                method: 'PUT'
              },
              response: {
                status: 204
              }
            }]);

            let saveBtn = projectItem.element(by.buttonText('Save & Close'));
            saveBtn.click();
          });

          it('closes the edit form', () => {
            expect(editForm).not.toBePresent();
          });
        });

        describe('cancelling edit form', () => {

          beforeEach(() => {
            let cancelBtn = projectItem.element(by.buttonText('Cancel'));
            cancelBtn.click();
          });

          it('closes the edit form', () => {
            expect(editForm).not.toBePresent();
          });
        });

        it ('displays the Slack integration form', () => {
          expect(orgPage.editWebhookForm).toBeDisplayed();
        });

        describe('and only a webhook URL is provided', () => {

          beforeEach(() => {
            orgPage.editWebhookUrl.sendKeys('http://some-url');
          });

          it('indicates a name is also required', () => {
            expect(orgPage.editWebhookNameField).toHaveClass('invalid');
          });

          it('is not submittable', () => {
            expect(orgPage.saveButton.isEnabled()).toBe(false);
          });
        });

        describe('and both webhook URL and name are provided', () => {

          beforeEach(() => {
            orgPage.editWebhookName.sendKeys('my-webhook');
            orgPage.editWebhookUrl.sendKeys('http://some-url');
          });

          describe('and the submit button is clicked', () => {

            beforeEach(() => {
              editBtn.click();
            });

            it('hides the new-project form', () => {
              expect(orgPage.editProjectForm.isPresent()).toBe(false);
            });
          });
        });

        describe('deleting a configured webhook', () => {
          beforeEach(() => {
            mocks.push({
              request: {
                url: '/api/v0/e/Chef/orgs/an_existing_org/projects/my_second_project/notifications/slack-webhook$',
                method: 'GET'
              },
              response: {
                body: {
                  webhook: {
                    url: "https://hooks.slack.com/services/FAKE/FAKE/THISISFAKE",
                    name: "#delivery-slack-tests",
                    enabled: true
                  }
                }
              }
            });
            mockApi(mocks);
            orgPage.get(params.forExistingOrg);
            editBtn.click();
          });

          describe('by clicking the trashcan button', () => {

            it('opens the delete confirmation modal', () => {
              orgPage.notificationTrashcan.click();
              expect(orgPage.removeSlackConfirmationModal.isPresent()).toBe(true);
              expect(orgPage.editProjectForm.isPresent()).toBe(true);
            });
          });
        });
      });
    });
  });

  describe('when a top-level github SCM link exists', () => {

    beforeEach(() => {

      mockApi(mocks);
      mockApi([{
        request: {
          url: '/scm-providers$',
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: [
                {
                  root_api_url: 'http:\/\/192.168.99.100:7990',
                  user_id: 'test-user',
                  _links: {
                    self: {
                      href: 'http:\/\/192.168.33.66\/api\/v0\/e\/Chef\/scm\/github\/servers\/http%3A%2F%2F192.168.99.100%3A7990'
                    }
                  }
                }
              ]
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: []
            }
          ]
        }
      }]);

      orgPage.get(params.forExistingOrg);
    });

    describe('changing SCM providers', () => {

      describe('from githubV2 project', () => {

          let editBtn, editForm, existingProject;
          beforeEach(() => {
            existingProject = orgPage.projectCards.get(2);
            editBtn = existingProject.element(by.css('.edit-project'));
            editBtn.click();
            editForm = existingProject.element(by.css('.edit-project-form'));
          });

          describe('githubV2 -> githubV2 (editing)', () => {

            it('marks the GitHub option as active', () => {
              let githubOption = editForm.element(by.cssContainingText('a', 'GitHub'));
              expect(githubOption).toHaveClass('active');
            });

            it('displays the github fields', () => {
              let formFields = orgPage.editProjectSCMRepoOwner;
              expect(formFields).toBeDisplayed();
            });
        });
      });

      describe('from local project', () => {

        describe('local -> githubV2', () => {

          let editBtn, editForm, existingProject;
          beforeEach(() => {
            existingProject = orgPage.projectCards.last();
            editBtn = existingProject.element(by.css('.edit-project'));
            editBtn.click();
            editForm = existingProject.element(by.css('.edit-project-form'));

            let githubOption = editForm.element(by.cssContainingText('a', 'GitHub'));
            githubOption.click();
          });

          it('enables the save button', () => {
            let saveBtn = editForm.element(by.buttonText('Save & Close'));
            expect(saveBtn.isEnabled()).toBe(true);
          });

          it('displays the github form fields', () => {
            let formFields = orgPage.editProjectSCMRepoOwner;
            expect(formFields).toBeDisplayed();
          });
        });
      });
    });
  });

  describe('when no top-level github SCM link exists', () => {

    beforeEach(() => {

      mockApi(mocks);
      mockApi([{
        request: {
          url: '/scm-providers$',
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: []
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: []
            }
          ]
        }
      }]);

      orgPage.get(params.forExistingOrg);
    });

    describe('changing SCM providers', () => {

      describe('from local project', () => {

        describe('local -> githubV2', () => {

          let editBtn, editForm, existingProject;
          beforeEach(() => {
            existingProject = orgPage.projectCards.last();
            editBtn = existingProject.element(by.css('.edit-project'));
            editBtn.click();
            editForm = existingProject.element(by.css('.edit-project-form'));

            let githubOption = editForm.element(by.cssContainingText('a', 'GitHub'));
            githubOption.click();
          });

          it('does not display the github form fields', () => {
            let formFields = orgPage.editProjectSCMRepoOwner;
            expect(formFields).not.toBePresent();
          });

          it('displays a helpful message', () => {
            let errorMsg = editForm.element(by.css('.project-scm-instructions'));
            expect(errorMsg).toBePresent();
          });
        });
      });
    });
  });

  describe('when projects exist with a top-level bitbucket SCM link', () => {

    beforeEach(() => {

      mockApi(mocks);
      mockApi([{
        request: {
          url: '/scm-providers$',
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: [true]
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: [
                {
                  root_api_url: 'http:\/\/192.168.99.100:7990',
                  user_id: 'scottopherson',
                  _links: {
                    self: {
                      href: 'http:\/\/192.168.33.66\/api\/v0\/e\/Chef\/bitbucket-servers\/http%3A%2F%2F192.168.99.100%3A7990'
                    }
                  }
                }
              ]
            }
          ]
        }
      }]);

      orgPage.get(params.forExistingOrg);
    });

    describe('changing SCM providers', () => {

      describe('from local project', () => {

        describe('local -> bitbucket', () => {

          let editBtn, editForm, bitbucketProject;
          beforeEach(() => {
            bitbucketProject = orgPage.projectCards.first();
            editBtn = bitbucketProject.element(by.css('.edit-project'));
            editBtn.click();
            editForm = bitbucketProject.element(by.css('.edit-project-form'));

            let bitbucketOption = editForm.element(by.cssContainingText('a', 'Bitbucket'));
            bitbucketOption.click();
          });

          it('enables the save button', () => {
            let saveBtn = editForm.element(by.buttonText('Save & Close'));
            expect(saveBtn.isEnabled()).toBe(true);
          });

          it('displays the bitbucket form fields', () => {
            let formFields = orgPage.editProjectSCMProjectKey;
            expect(formFields).toBeDisplayed();
          });
        });
      });
    });
  });

  describe('when no projects exist', () => {

    beforeEach(() => {

      mockApi(mocks);
      mockApi([{
        request: {
          url: '/scm-providers$',
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: [true]
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: []
            }
          ]
        }
      }]);

      orgPage.get(params.forNewOrg);
    });

    it('shows the new-project form', () => {
      expect(orgPage.newProjectForm).toBeDisplayed();
    });

    it('hides the project filter', () => {
      expect(orgPage.projectFilter).not.toBeDisplayed();
    });
  });

  describe('new project form', () => {

    beforeEach(() => {
      mockApi(mocks);
      mockApi([{
        request: {
          url: '/scm-providers$',
          method: 'GET'
        },
        response: {
          body: [
            {
              name: 'Chef Automate',
              type: 'local',
              projectCreateUri: '/projects',
              scmSetupConfigs: [true]
            },
            {
              name: 'GitHub',
              type: 'github',
              projectCreateUri: '/github-projects',
              verify_ssl: true,
              scmSetupConfigs: [true]
            },
            {
              name: 'Bitbucket',
              type: 'bitbucket',
              projectCreateUri: '/bitbucket-projects',
              scmSetupConfigs: []
            }
          ]
        }
      }]);
      orgPage.get(params.forExistingOrg);
    });

    describe('when the new-project button is clicked', () => {

      beforeEach(() => {
        orgPage.newProjectButton.click();
      });

      it('shows the new-project form', () => {
        expect(orgPage.newProjectForm).toBeDisplayed();
      });

      describe('when one or more external SCM options is available', () => {

        it('shows all available SCM providers', () => {
          expect(orgPage.scmOptions.count()).toEqual(3);
        });

        it('defaults to Automate', () => {
          expect(orgPage.scmOptions.first().getText()).toBe('Chef Automate');
        });

        describe('selecting github', () => {
          beforeEach(() => {
            orgPage.scmOption('GitHub').click();
          });

          it('displays the github form', () => {
            expect(orgPage.githubFields).toBeDisplayed();
          });
        });

        describe('selecting bitbucket', () => {
          beforeEach(() => {
            orgPage.scmOption('Bitbucket').click();
          });

          describe('when scm is not configured', () => {
            it('displays instructions to configure bitbucket', () => {
              expect(orgPage.bitbucketInstructions).toBeDisplayed();
              expect(orgPage.saveButton.isEnabled()).toBe(false);
            });
          });

          describe('when scm is configured', () => {
            beforeEach(() => {
              mockApi.clear();
              mockApi(mocks);
              mockApi([{
                request: {
                  url: '/scm-providers$',
                  method: 'GET'
                },
                response: {
                  body: [
                    {
                      name: 'Chef Automate',
                      type: 'local',
                      projectCreateUri: '/projects',
                      scmSetupConfigs: [true]
                    },
                    {
                      name: 'GitHub',
                      type: 'github',
                      projectCreateUri: '/github-projects',
                      verify_ssl: true,
                      scmSetupConfigs: [true]
                    },
                    {
                      name: 'Bitbucket',
                      type: 'bitbucket',
                      projectCreateUri: '/bitbucket-projects',
                      scmSetupConfigs: [
                        {
                          root_api_url: 'https://bitbucket.my.co',
                          user_id: 'scottopherson',
                          _links: {
                            self: {
                              href: 'http://delivery-server/api/v0/e/cd/bitbucket-servers/https%3A%2F%2Fbitbucket.my.co'
                            }
                          }
                        }
                      ]
                    }
                  ]
                }
              }]);

              orgPage.get(params.forExistingOrg);
              orgPage.newProjectButton.click();
              orgPage.scmOption('Bitbucket').click();
            });

            it('displays the bitbucket form', () => {
              expect(orgPage.bitbucketFields).toBeDisplayed();
              expect(orgPage.saveButton.isEnabled()).toBe(false);
            });

            describe('when the form is filled out correctly and save is clicked', () => {
              beforeEach(() => {
                mockApi([{
                  request: {
                    url: '/bitbucket-projects$',
                    method: 'POST'
                  },
                  response: {
                    status: 204
                  }
                }]);
                orgPage.projectNameField.sendKeys('test_project');
                orgPage.bitbucketProjectKeyField.sendKeys('TEST');
                orgPage.bitbucketRepositoryNameField.sendKeys('test_repo');
                orgPage.bitbucketPipelineBranchField.sendKeys('master');
                orgPage.saveButton.click();
              });

              it('should stay on the orgs page', () => {
                expect(browser.getCurrentUrl())
                  .toMatch(/.*\/organizations\/an_existing_org$/);
              });
            });
          });
        });
      });

      it ('displays the Slack integration form', () => {
        expect(orgPage.newWebhookForm).toBeDisplayed();
      });

      describe('and only a webhook URL is provided', () => {

        beforeEach(() => {
          orgPage.newWebhookUrl.sendKeys('http://some-url');
        });

        it('indicates a name is also required', () => {
          expect(orgPage.newWebhookNameField).toHaveClass('invalid');
        });

        it('is not submittable', () => {
          expect(orgPage.saveButton.isEnabled()).toBe(false);
        });
      });

      describe('and both webhook URL and name are provided', () => {

        beforeEach(() => {
          orgPage.newWebhookName.sendKeys('my-webhook');
          orgPage.newWebhookUrl.sendKeys('http://some-url');
        });

        describe('and the submit button is clicked', () => {

          beforeEach(() => {
            orgPage.projectNameField.sendKeys('Mitchell');
            orgPage.saveButton.click();

            mocks.push(
              {
                request: {
                  url: `/api/v0/e/Chef/orgs/an_existing_org/projects$`,
                  method: 'POST'
                },
                response: {
                  status: 201,
                  body: {}
                }
              },
              {
                request: {
                  url: '/api/v0/e/Chef/orgs/a_new_org/projects/Mitchell/notifications/slack-webhook',
                  method: 'PUT',
                  body: {
                    name: 'my-webhook',
                    url: 'http://some-url'
                  }
                },
                response: {
                  status: 201
                }
              }
            );
            mockApi(mocks);
          });

          it('hides the new-project form', () => {
            expect(orgPage.newProjectForm.isPresent()).toBe(false);
          });
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

            it('shows a success message', () => {
              orgPage.newWebhookTestButton.click();
              expect(orgPage.newWebhookTestSuccessMessage).toBePresent();
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

            it('shows a failure message', () => {
              orgPage.newWebhookTestButton.click();
              expect(orgPage.newWebhookTestErrorMessage).toBePresent();
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

            it('shows a failure message', () => {
              orgPage.newWebhookTestButton.click();
              expect(orgPage.newWebhookTestErrorMessage).toBePresent();
            });
          });
        });
      });
    });
  });
});
