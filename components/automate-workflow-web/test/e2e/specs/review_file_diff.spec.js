import ReviewFileDiffPage from '../page_objects/review_file_diff.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('review file diff page', () => {
  let reviewFileDiffPage, params;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    params = {
      org: 'chef_delivery',
      project: 'delivery',
      change: 'b2d440d1-0ed8-426e-92cf-73fc1659a95c',
      file: 'foo.rb',
      start: 'base',
      end: 'p1'
    };

    mockApi([
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
          url: `/api/v0/e/Chef/orgs/${params.org}$`,
          method: 'GET'
        },
        response: {
          body: {
            name: params.org,
            project_count: 0
          }
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects$`,
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}$`,
          method: 'GET'
        },
        response: {
          body: {
            'name': params.project,
            'ent_name': 'Chef',
            'org_name': 'orgs',
            'git_url': 'ssh:my_new_project'
          }
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/changes$`,
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/pipelines$`,
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
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/dependencies$`,
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
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/changes/${params.change}$`,
          method: 'GET'
        },
        response: {
          body: {
            id: params.change,
            title: 'Change A',
            description: '',
            topic: 'change-a',
            target: 'master',
            state: 'open',
            submit_at: '2015-08-13 15:22:17',
            submit_by: 'scottopherson',
            stages:[
              {
                stage: 'verify',
                status: 'running',
                phases:[
                  {
                    phase: 'unit',
                    status: 'passed',
                    href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33166',
                    search_query: 'recipes:delivery_builder',
                    search_description: null
                  },
                  {
                    phase: 'lint',
                    status: 'passed',
                    href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33167',
                    search_query: 'recipes:delivery_builder',
                    search_description: null
                  },
                  {
                    phase: 'syntax',
                    status: 'running',
                    href: '/api/v0/e/Chef/orgs/Chef_Delivery/projects/delivery/pipelines/master/phase_runs/33168',
                    search_query: 'recipes:delivery_builder',
                    search_description: null
                  }
                ]
              }
            ],
            patchsets: [
              {
                sequence_number: 1,
                sha: 'c08b85d02cd7541645910f1fe6d02b5166487568',
                submitted_at: '2015-08-13 20:08:51',
                stats: {
                  insertions: 475,
                  deletions: 46
                },
                files_changed:[
                  ['M', 'foo.rb']
                ],
                commit_msgs:[
                  [
                    'Add awesome change',
                    '',
                    'c08b85d02cd7541645910f1fe6d02b5166487568'
                  ]
                ]
              }
            ]
          }
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/changes/${params.change}/comments`,
          method: 'GET'
        },
        response: {
          body: []
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/changes/${params.change}/compare?context=3&start=${params.start}&end=${params.end}$`,
          method: 'GET'
        },
        response: {
          body: {
            diff: {
              content: '',
              end_sha: '0151d8ab9bdbbf37cd8809296c1fdc9de1906d07',
              start_sha: 'f02ce895c31ea617689cd3ea7a75965e4af730a2'
            }
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
      }
    ]);

    reviewFileDiffPage = new ReviewFileDiffPage();
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

  it('should display a back button', () => {
    reviewFileDiffPage.get(params);

    expect(reviewFileDiffPage.backBtn).toBeDisplayed();
  });

  describe('clicking the back button', () => {

    it('should go back to the review overview page', () => {
      reviewFileDiffPage.get(params);
      reviewFileDiffPage.backBtn.click();

      expect(browser.getCurrentUrl()).toMatch(/review/);
    });
  });
});
