import ReviewPage from '../page_objects/review.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('review page', () => {

  let reviewPage, params;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    params = {
      org: 'chef_delivery',
      project: 'delivery',
      change: 'b2d440d1-0ed8-426e-92cf-73fc1659a95c'
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
            promotion: {
              status: 'proceed',
            },
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
          body: [
            {
              id: 1,
              patchset: 1,
              type: 'patchset',
              content: 'First comment',
              status: 'published',
              datetime: '2016-01-01 00:00:00',
              author: {
                name: 'john',
                first: 'John',
                last: 'Doe'
              }
            },
            {
              id: 2,
              patchset: 1,
              type: 'patchset',
              content: 'Second comment',
              status: 'published',
              datetime: '2016-01-02 00:00:00',
              author: {
                name: 'john',
                first: 'John',
                last: 'Doe'
              }
            },
            {
              id: 3,
              patchset: 1,
              type: 'patchset',
              content: 'Third comment',
              status: 'published',
              datetime: '2016-01-03 00:00:00',
              author: {
                name: 'john',
                first: 'John',
                last: 'Doe'
              }
            }
          ]
        }
      },
      {
        request: {
          url: `/api/v0/e/Chef/orgs/${params.org}/projects/${params.project}/changes/${params.change}/comments`,
          method: 'POST'
        },
        response: {
          status: 201
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

    reviewPage = new ReviewPage();
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

  describe('comments', () => {
    let comments;

    beforeEach(() => {
      reviewPage.get(params);
      comments = element.all(by.css('.comment'));
    });

    it('displays comments', () => {
      expect(comments.count()).toEqual(3);
      comments.each((comment) => {
        expect(comment).toBeDisplayed();
      });
    });

    it('sorts comments from oldest to newest', () => {
      expect(comments.first()).toContainText('First comment');
      expect(comments.last()).toContainText('Third comment');
    });
  });

  describe('posting comments', () => {

    it('add comment button should post comment', () => {
      reviewPage.get(params);

      reviewPage.commentEditorTextArea.sendKeys('test comment');

      // Check the model to ensure the text was entered.
      expect(reviewPage.commentForm.evaluate('newComment.content')).toBe('test comment');
      reviewPage.addCommentBtn.click();

      // On success on the scope sets the value back to null.
      expect(reviewPage.commentForm.evaluate('newComment.content')).toBe(null);
    });

    it('command+enter or ctrl+enter should post comment', () => {
      reviewPage.get(params);
      reviewPage.commentEditorTextArea.sendKeys('test comment');

      // Check the model to ensure the text was entered.
      expect(reviewPage.commentForm.evaluate('newComment.content')).toBe('test comment');

      reviewPage.sendCommandCtrlEnter();

      // On success on the scope sets the value back to null.
      expect(reviewPage.commentForm.evaluate('newComment.content')).toBe(null);
    });
  });
});
