import AdminPage from '../page_objects/admin.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('admin page', () => {
  let adminPage;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    mockApi([
      authorizedLogin,
      {
        request: {
          url: '/api/v0/e/Chef/users$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {
            users: []
          }
        }
      },
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
          url: '/api/v0/e/Chef/searches$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {
            search: ''
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
              'grrm',
              'builder',
              'internal',
              'external',
              'samltest'
            ],
            _links: {
              self: {
                'href':'/api/v0/e/cd/users'
              },
              user: {
                href:'/api/v0/e/cd/users/{user_name}',
                templated:true
              }
            }
          }
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/bitbucket-servers$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {}
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/saml/config$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {}
        }
      },
      {
        request: {
          url: '/api/v0/e/Chef/notifications/smtp$',
          method: 'GET'
        },
        response: {
          status: 200,
          body: {}
        }
      }
    ]);

    adminPage = new AdminPage();
    adminPage.get();
  });

  it('shows users on the admin page', () => {
    expect(adminPage.userTab).toBeDisplayed();
  });

  it('shows SCM Setup on the admin page', () => {
    expect(adminPage.scmSetupTab).toBeDisplayed();
  });

  it('shows Email Setup on the admin page', () => {
    expect(adminPage.emailSetupTab).toBeDisplayed();
  });
});
