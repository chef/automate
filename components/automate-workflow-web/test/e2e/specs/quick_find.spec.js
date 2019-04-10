import OrgsPage from '../page_objects/orgs.po';
import QuickFind from '../page_objects/quick_find.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('quick find', () => {
  let orgsPage;
  let quickFind;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    orgsPage = new OrgsPage();
    quickFind = new QuickFind();

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
          url: '/api/v0/e/Chef/pipelines$',
          method: 'GET'
        },
        response: {
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
      }
    ]);

    orgsPage.get();
  });

  describe('when the search term has results', () => {

    it('should show the results', () => {
      quickFind.input.sendKeys('foo');
      expect(quickFind.resultsDropdown).toBeDisplayed();
      expect(quickFind.results.count()).toEqual(1);
      quickFind.input.clear();
      quickFind.input.sendKeys('Ba');
      expect(quickFind.resultsDropdown).toBeDisplayed();
      expect(quickFind.results.count()).toEqual(2);
    });
  });

  describe('when the search term has no results', () => {

    it('should not show any results', () => {
      quickFind.input.sendKeys('rawrrrrrrrr');
      expect(quickFind.resultsDropdown).not.toBeDisplayed();
    });
  });
});
