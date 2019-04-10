import UsersPage from '../page_objects/users.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('Users page', () => {

  let usersPage, mocks;

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
            users:[
              'grrm',
              'builder',
              'internal',
              'external',
              'samltest'
            ],
            _links:{
              self:{'href':'/api/v0/e/cd/users'
            },
            user:{
              href:'/api/v0/e/cd/users/{user_name}',
              templated:true}
            }
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
          body: []
        }
      }
    ];
    usersPage = new UsersPage();
  });

  describe('creating a new user', () => {

    beforeEach(() => {
      mockApi(mocks);

      usersPage.get();
      usersPage.newUserButton.click();
    });

    it('should show the new-user form', () => {
      expect(usersPage.newUserForm).toBeDisplayed();
    });

    describe('and the submit button is clicked', () => {
      beforeEach(() => {
        mocks = mocks.concat([
          {
            request: {
              url: `/api/v0/e/Chef/internal-users$`,
              method: 'POST'
            },
            response: {
              status: 201,
              body: {}
            }
          },
          {
            request: {
              url: `/api/v0/e/Chef/internal-users/testUser/change-password$`,
              method: 'POST'
            },
            response: {
              status: 204,
              body: {}
            }
          },
          {
            request: {
              url: `/api/v0/e/Chef/authz/users/testUser$`,
              method: 'POST'
            },
            response: {
              status: 200,
              body: {}
            }
          },
          {
            request: {
              url: `/api/v0/e/Chef/users/testUser$`,
              method: 'GET'
            },
            response: {
              status: 200,
              body: {}
            }
          }
        ]);
        mockApi(mocks);
      });

      it('hides the new-user form', () => {
        usersPage.newUserFirst.sendKeys('testFirst');
        usersPage.newUserLast.sendKeys('testLast');
        usersPage.newUserEmail.sendKeys('test@email.com');
        usersPage.newUserName.sendKeys('testUser');
        usersPage.newUserPassword.sendKeys('testPassword');
        usersPage.createUserButton.click();
        expect(usersPage.newUserForm).not.toBePresent();
      });
    });
  });

  describe('Users list', () => {

    beforeEach(() => {
      mocks.push(
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
            ssh_pub_key: 'cool_pub_key',
            type : 'internal',
            _links: {
              self: {
                href: '/api/v0/e/foocorp/users/someone'
              },
              'change-password': {
                href: '/api/v0/e/foocorp/internal-users/someone/change-password'
              }
            }
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
      });
      mockApi(mocks);
      usersPage.get();
    });

    it('should display a list of all Users within the enterprise', () => {
      expect(usersPage.usersList.count()).toBe(5);
    });

    describe('clicking the edit button', () => {

      it('toggles the edit form', () => {
        usersPage.editUserButton.click();
        expect(usersPage.editUserForm).toBeDisplayed();

        usersPage.editUserButton.click();
        expect(usersPage.editUserForm).not.toBePresent();
      });
    });

    describe('editing internal user', () => {

      beforeEach(() => {
        usersPage.editUserButton.click();
      });

      it('shows usertype buttonbar', () =>{
        expect(usersPage.buttonBarUserType.isPresent()).toBe(true);
      });

      // Unable to test showing the user password unless we update the json schema
      // to not have dashes
      // it('shows the password field for an internal user', () => {
      //   expect(usersPage.editUserPassword.isPresent()).toBe(true);
      // });

      describe('switching user auth type', () =>{
        beforeEach(() =>  {
            usersPage.samlUserButton.click();
        });

        it('hides the password field when not an internal user', () =>{
          expect(usersPage.editUserPassword.isPresent()).toBe(false);
        });

        it('disables editing name & email fields', () => {
          usersPage.samlUserButton.click();
          expect(usersPage.editUserFirst.isEnabled()).toBe(false);
          expect(usersPage.editUserLast.isEnabled()).toBe(false);
          expect(usersPage.editUserEmail.isEnabled()).toBe(false);
        });

        it('opens a confirmation Modal upon saveAndClose', () =>{
          usersPage.samlUserButton.click();
          expect(usersPage.userTypeChangeModal.isPresent()).toBe(false);
          usersPage.createUserButton.click();
          expect(usersPage.userTypeChangeModal.isPresent()).toBe(true);
          usersPage.modalCancelButton.click();
        });

        // Unable to test showing the user password unless we update the json schema
        // to not have dashes
        // it('shows the password field for an internal user', () => {
        //   expect(usersPage.editUserPassword.isPresent()).toBe(true);
        // });

        describe('switching user auth type', () =>{
          beforeEach(() =>  {
              usersPage.samlUserButton.click();
          });

          it('hides the password field when not an internal user', () =>{
            expect(usersPage.editUserPassword.isPresent()).toBe(false);
          });

          it('disables editing name & email fields', () => {
            usersPage.samlUserButton.click();
            expect(usersPage.editUserFirst.isEnabled()).toBe(false);
            expect(usersPage.editUserLast.isEnabled()).toBe(false);
            expect(usersPage.editUserEmail.isEnabled()).toBe(false);
          });

          it('opens a confirmation Modal upon saveAndClose', () =>{
            usersPage.samlUserButton.click();
            expect(usersPage.userTypeChangeModal.isPresent()).toBe(false);
            usersPage.createUserButton.click();
            expect(usersPage.userTypeChangeModal.isPresent()).toBe(true);
            usersPage.modalCancelButton.click();
          });
        });
      });
    });
  });
});
