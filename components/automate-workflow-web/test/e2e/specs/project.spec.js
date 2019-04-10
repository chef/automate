import ProjectPage from '../page_objects/project.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('project page', () => {

  let projectPage, mocks;

  let params = {
    org: 'TestOrg',
    project: 'TestProject'
  };

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
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
          url: '/api/v0/e/Chef/orgs/TestOrg$',
          method: 'GET'
        },
        response: {
          body: {
            name: 'TestOrg',
            project_count: 0
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects$',
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/TestProject$',
          method: 'GET'
        },
        response: {
          body: {
            'name': 'TestProject',
            'ent_name': 'Chef',
            'org_name': 'orgs',
            'git_url': 'ssh:my_new_project',
            'scm': {
              'type': 'local'
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectA$',
          method: 'GET'
        },
        response: {
          body: {
            'name': 'ProjectA',
            'ent_name': 'Chef',
            'org_name': 'TestOrg',
            'git_url': 'ssh:projecta',
            'scm': {
              'type': 'local'
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectB$',
          method: 'GET'
        },
        response: {
          body: {
            'name': 'ProjectB',
            'ent_name': 'Chef',
            'org_name': 'TestOrg',
            'git_url': 'ssh:projectb',
            'scm': {
              'type': 'local'
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectC$',
          method: 'GET'
        },
        response: {
          body: {
            'name': 'ProjectC',
            'ent_name': 'Chef',
            'org_name': 'TestOrg',
            'git_url': 'ssh:projectc',
            'scm': {
              'type': 'local'
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/.*/changes$',
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/.*/pipelines$',
          method: 'GET'
        },
        response: {
          body: {
            pipelines: []
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/TestProject/dependencies$',
          method: 'GET'
        },
        response: {
          body: {
            dependencies: {
              master: [
                {
                  enterprise: 'Chef',
                  organization: 'TestOrg',
                  project: 'ProjectB',
                  pipeline: 'master'
                }
              ],
              'not-master': [
                {
                  enterprise: 'Chef',
                  organization: 'TestOrg',
                  project: 'ProjectD',
                  pipeline: 'master'
                }
              ]
            },
            required_by: {
              master: [
                {
                  enterprise: 'Chef',
                  organization: 'TestOrg',
                  project: 'ProjectC',
                  pipeline: 'master'
                }
              ]
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/orgs/TestOrg/projects/.*/dependencies$',
          method: 'GET'
        },
        response: {
          body: {
            dependencies: [],
            required_by: []
          }
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
      }];

    projectPage = new ProjectPage();
  });

  describe('dependencies', () => {

    beforeEach(() => {
      mockApi(mocks);
      projectPage.get(params);
    });

    it('should show correct count of dependencies', () => {
      projectPage.dependenciesTab.click();
      expect(projectPage.dependenciesTabCount.getText()).toBe('2');
      expect(projectPage.dependencyList.count()).toBe(2);
    });

    it('should link to dependencies', () => {
      projectPage.dependenciesTab.click();
      element(by.css('.dependency:first-child a')).click();
      expect(projectPage.projectName.getText()).toBe('ProjectB');
    });

    it('should show correct count of consumers', () => {
      projectPage.consumersTab.click();
      expect(projectPage.requiredByTabCount.getText()).toBe('1');
      expect(projectPage.requiredByList.count()).toBe(1);
    });

    it('should link to consumers', () => {
      projectPage.consumersTab.click();
      element(by.css('.consumer:first-child a')).click();
      expect(projectPage.projectName.getText()).toBe('ProjectC');
    });
  });

  describe('pipeline creation instructions', () => {

    beforeEach(() => {
      mockApi(mocks);
      projectPage.get(params);
    });

    describe('before a pipeline has been created', () => {

      describe('when the project is backed by Delivery', () => {

        it('explains how to create one', () => {
          projectPage.pipelinesTab.click();
          expect(projectPage.pipelineCreationForm.isPresent()).toBe(false);
          expect(projectPage.pipelineCreationInstructions.isPresent()).toBe(true);
        });
      });
    });
  });

  describe('clone options', () => {

    beforeEach(() => {
      mocks = mocks.concat([
        {
          request: {
            url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectD$',
            method: 'GET'
          },
          response: {
            body: {
              'name': 'ProjectD',
              'ent_name': 'Chef',
              'org_name': 'TestOrg',
              'git_url': 'ssh:projecta',
              'scm': {
                'type': 'local'
              }
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectE$',
            method: 'GET'
          },
          response: {
            body: {
              'name': 'ProjectE',
              'ent_name': 'Chef',
              'org_name': 'TestOrg',
              'git_url': 'ssh:projectb',
              'scm': {
                'type': 'bitbucket'
              }
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectF$',
            method: 'GET'
          },
          response: {
            body: {
              'name': 'ProjectF',
              'ent_name': 'Chef',
              'org_name': 'TestOrg',
              'git_url': 'ssh:projectc',
              'scm': {
                'type': 'github'
              }
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs/TestOrg/projects/ProjectG$',
            method: 'GET'
          },
          response: {
            body: {
              'name': 'ProjectG',
              'ent_name': 'Chef',
              'org_name': 'TestOrg',
              'git_url': 'ssh:projectg',
              'scm': {
                'type': 'githubV2'
              }
            }
          }
        }

      ]);

      mockApi(mocks);
      projectPage.get(params);
    });

    describe('when the scm type is "local"', () => {

      beforeEach(() => {
        let params = {
          org: 'TestOrg',
          project: 'ProjectD'
        };

        projectPage.get(params);
        projectPage.cloneTab.click();
      });

      it('should show the clone options', () => {
        expect(projectPage.cloneOptions).toBePresent();
      });
    });

    describe('when the scm type is "bitbucket"', () => {

      beforeEach(() => {
        let params = {
          org: 'TestOrg',
          project: 'ProjectE'
        };

        projectPage.get(params);
        projectPage.cloneTab.click();
      });

      it('should show the clone options', () => {
        expect(projectPage.cloneOptions).toBePresent();
      });
    });

    describe('when the scm type is "github"', () => {

      beforeEach(() => {
        let params = {
          org: 'TestOrg',
          project: 'ProjectF'
        };

        projectPage.get(params);
      });

      it('does not show the clone tab', () => {
        expect(projectPage.cloneTab).not.toBePresent();
      });
    });

    describe('when the scm type is "githubV2"', () => {

      beforeEach(() => {
        let params = {
          org: 'TestOrg',
          project: 'ProjectG'
        };

        projectPage.get(params);
      });

      it('should show the clone options', () => {
        expect(projectPage.cloneTab).toBePresent();
      });
    });
  });

  describe('email integration', () => {

    describe('and an SMTP server has been configured', () => {

      beforeEach(() => {
        mocks = mocks.concat([{
            request: {
              url: '/api/v0/e/Chef/notifications/$',
              method: 'GET'
            },
            response: {
              body: {
                notifications: [
                  'smtp'
                ]
              }
            }
          },
          {
            request: {
              url: '/api/v0/e/Chef/orgs/TestOrg/projects/TestProject/notifications/watch$',
              method: 'PUT'
            },
            response: {
              status: 200
            }
          }
        ]);
      });

      describe('when the user is watching the project', () => {

        beforeEach(() => {
          mocks.push({
            request: {
              url: '/api/v0/e/Chef/orgs/TestOrg/projects/TestProject/notifications/watch$',
              method: 'GET'
            },
            response: {
              status: 200,
              body: {
                categories: [
                  'review',
                  'deliver',
                  'observe'
                ]
              }
            }
          });

          mockApi(mocks);
          projectPage.get(params);
        });

        describe('the watch button', () => {

          it('is present', () => {
            expect(projectPage.watchButton).toBePresent();
          });

          it('is enabled', () => {
            expect(projectPage.watchButton.getAttribute('disabled')).toBe(null);
          });

          it('uses the active icon', () => {
            expect(projectPage.watchButton.getAttribute('icon-button')).toEqual('eyeball');
          });

          it('toggles the watch menu', () => {
            projectPage.watchButton.click();
            expect(projectPage.watchMenu).toBePresent();

            projectPage.watchButton.click();
            expect(projectPage.watchMenu).not.toBePresent();
          });
        });

        describe('the watch menu', () => {

          it('is not present', () => {
            expect(projectPage.watchMenu).not.toBePresent();
          });

          describe('when opened', () => {

            beforeEach(() => {
              projectPage.watchButton.click();
            });

            it('shows the list of watchable categories', () => {
              expect(projectPage.watchCategories.get(0).getText()).toBe('Review');
              expect(projectPage.watchCategories.get(1).getText()).toBe('Deliver');
              expect(projectPage.watchCategories.get(2).getText()).toBe('Observe');
            });

            it('shows the subscribed-to categories as selected', () => {
              expect(projectPage.categoryCheckbox('review').getAttribute('checked')).toBe('true');
              expect(projectPage.categoryCheckbox('deliver').getAttribute('checked')).toBe('true');
              expect(projectPage.categoryCheckbox('observe').getAttribute('checked')).toBe('true');
            });
          });
        });
      });

      describe('when the user is not watching the project', () => {

        beforeEach(() => {
          mocks.push({
            request: {
              url: '/api/v0/e/Chef/orgs/TestOrg/projects/TestProject/notifications/watch$',
              method: 'GET'
            },
            response: {
              status: 200,
              body: {
                categories: []
              }
            }
          });

          mockApi(mocks);
          projectPage.get(params);
        });

        describe('the watch button', () => {

          it('is enabled', () => {
            expect(projectPage.watchButton.getAttribute('disabled')).toBe(null);
          });

          it('uses the inactive icon', () => {
            expect(projectPage.watchButton.getAttribute('icon-button')).toEqual('eyeball-off');
          });
        });
      });
    });

    describe('when an SMTP server has not been configured', () => {

      beforeEach(() => {
        mocks.push({
          request: {
            url: '/api/v0/e/Chef/notifications/$',
            method: 'GET'
          },
          response: {
            body: {
              notifications: []
            }
          }
        });

        mockApi(mocks);
        projectPage.get(params);
      });

      describe('the watch button', () =>{

        it('is disabled', () => {
          expect(projectPage.watchButton.getAttribute('disabled')).toEqual('true');
        });
      });
    });
  });
});
