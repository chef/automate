import { browser, $, ExpectedConditions as EC } from 'protractor';
import { fakeServer } from './helpers/fake_server';
import {
  expectCountElements,
  waitForElement,
  waitForSpinners
} from './helpers/browser_expectations';

describe('Admin pages', () => {
  describe('User Management', () => {
    beforeEach(() => {
      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .many()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V1',
            'minor': 'V0'
          }
        }));

      fakeServer()
        .get('/api/v0/auth/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/auth/users': {
                get: true,
                put: false,
                post: true,
                delete: false,
                patch: false
              }
            }
          }
        ));

      fakeServer()
        .get('/api/v0/auth/users')
        .many()
        .reply(200, JSON.stringify(
          {
            users: [
              {
                id: 'b369ef15-6323-4c31-bcbc-23fb0c9ba55d',
                name: 'Local Administrator',
                email: 'admin',
                username: 'admin'
              }
            ]
          }
        ));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/users');
    });

    it('displays heading', () => {
      const heading = $('chef-heading');
      browser.wait(EC.textToBePresentInElement(heading, 'Users'), 5000);
    });

    it('displays users table', () => {
      expectCountElements('chef-table-new chef-table-header chef-table-row', 1);
      // one row for the admin user in the table body
      expectCountElements('chef-table-new chef-table-body chef-table-row', 1);
    });

    it('displays create user button', () => {
      const addButton = $('.page-body chef-button');
      browser.wait(EC.textToBePresentInElement(addButton, 'Create User'), 5000);
    });

    it('displays the returned user info', () => {
      const fullname = $('chef-table-new chef-table-body chef-table-row chef-table-cell:first-child a');
      browser.wait(EC.textToBePresentInElement(fullname, 'Local Administrator'), 5000);

      const username = $('chef-table-new chef-table-body chef-table-row chef-table-cell:nth-child(2)');
      browser.wait(EC.textToBePresentInElement(username, 'admin'), 5000);
    });

    describe('control button', () => {
      it('is displayed', () => {
        waitForElement('chef-table-new chef-table-body chef-table-row chef-control-menu')
        .then((e) => {
          expect(e.isDisplayed()).toBeTruthy();
        });
      });

      ['Delete User'].forEach((item, index) => {
        it(`when clicked, shows ${item}`, () => {
          const menu = $('chef-table-new chef-table-body chef-table-row chef-control-menu');
          browser.wait(EC.elementToBeClickable(menu), 5000).then(() => {
            menu.click().then(() => {
              const dropDownOption = menu.$(`chef-option:nth-child(${index + 1}`);
              const dropDownOpened = () => dropDownOption.getText().then(val => val === item);
              browser.wait(dropDownOpened, 100, 'Control options should render.');
            });
          });
        });
      });
    });
  });

  describe('Policies list', () => {
    beforeEach(() => {
      fakeServer()
        .get('/api/v0/auth/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/iam/v2/policies': {
                get: true,
                put: false,
                post: true,
                delete: false,
                patch: false
              }
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/policies')
        .many()
        .reply(200, JSON.stringify(
          {
            policies: [
              {
                id: 'some-policy-id',
                name: 'Some policy whose name does not start with A',
                members: [],
                type: 'CUSTOM'
              },
              {
                id: 'chef-managed-administrator',
                name: 'Administrator All Access',
                members: ['team:local:admins'],
                type: 'CHEF_MANAGED'
              }
            ]
          }
        ));

      // Note(sr): Technically, this isn't required: the crucial check in the
      // policy-list component only checks '=== "v1"' and '!== "v1"', and
      // undefined, or whatever it ends up being when the endpoint is not found,
      // is unequal "v1". But this is more correct, and less dependant on that
      // specific detail.
      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .many()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V2',
            'minor': 'V0'
          }
        }));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/policies');
    });

    it('displays heading', () => {
      waitForElement('chef-heading').then((heading) => {
        expect(heading.getText()).toBe('Policies');
      });
    });

    it('displays policy list component', () => {
      waitForElement('app-policy-list').then((policyList) => {
        expect(policyList).not.toBeNull();
      });
    });

    describe('displays the alphabetically sorted policies', () => {
      it('first policy', () => {
        waitForElement('app-policy-list chef-table-new chef-table-row:nth-child(1) chef-table-cell:first-child a')
          .then((name) => {
            expect(name.getText()).toBe('Administrator All Access');
          });

        waitForElement('chef-table-new chef-table-row:nth-child(1) chef-table-cell:nth-child(2)')
          .then((policyType) => {
            expect(policyType.getText()).toBe('Chef-managed');
          });

        waitForElement('app-policy-list chef-table-new chef-table-row:nth-child(1) chef-table-cell:nth-child(3)')
          .then((members) => {
            expect(members.getText()).toBe('In use');
          });
      });

      it('second policy', () => {
        waitForElement('app-policy-list chef-table-new chef-table-row:nth-child(2) chef-table-cell:first-child a')
          .then(name => {
            expect(name.getText()).toBe('Some policy whose name does not start with A');
          });

        waitForElement('chef-table-new chef-table-row:nth-child(2) chef-table-cell:nth-child(2)')
        .then(policyType => {
          expect(policyType.getText()).toBe('Custom');
        });

        waitForElement('app-policy-list chef-table-new chef-table-row:nth-child(2) chef-table-cell:nth-child(3)')
          .then(members => {
            expect(members.getText()).toBe('No members');
          });
      });
    });

    describe('control menu', () => {
      it('is not displayed for chef-managed policies', () => {
        // admin policy row
        expect(
          $('chef-table-new chef-table-body chef-table-row:nth-child(1) chef-control-menu')
            .isPresent()).toBeFalsy();
      });

      it('is displayed for custom policies', () => {
        // custom policy row
        waitForElement('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-control-menu')
          .then(controlButton => {
            expect(controlButton.isPresent()).toBeTruthy();
          });
      });

      ['Delete Policy'].forEach((item, index) => {
        it(`when clicked, shows ${item}`, () => {
          waitForElement('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-control-menu')
            .then(controlButton => {
              browser.wait(EC.elementToBeClickable(controlButton));
              controlButton.click().then(() => {
                const dropDownOption = controlButton.$(`chef-option:nth-child(${index + 1}`);
                const dropDownOpened = () => dropDownOption.getText()
                  .then(val => val === item);
                browser.wait(dropDownOpened, 100, 'Control options should render.');
              });
            });
        });
      });

      it('after delete clicked, policy removed from list once', () => {
        fakeServer()
          .delete('/apis/iam/v2/policies/some-policy-id')
          .many()
          .reply(200);

        waitForElement('chef-table-new chef-table-body chef-table-row:nth-child(2)')
        .then(somePolicy => {
          // open control menu
          const controlButton = somePolicy.$('chef-control-menu');
          browser.wait(EC.elementToBeClickable(controlButton));
          controlButton.click().then(() => {
            // select Delete Policy
            const deleteOption = controlButton.$('chef-option:nth-child(1)');
            browser.wait(EC.visibilityOf(deleteOption), 5000, 'Delete option should render');
            deleteOption.click();

            const deleteModal = $('app-delete-object-modal');
            browser.wait(EC.visibilityOf(deleteModal), 5000, 'Delete confirm modal should render');
            // confirm
            deleteModal.$('chef-button:first-child button').click();

            browser.wait(EC.not(EC.presenceOf(somePolicy)), 5000, 'Deleted policy should be gone');
          });
        });
      });
    });
  });

  describe('Policy details', () => {
    beforeEach(() => {
      fakeServer()
        .get('/apis/iam/v2/policies/some-test-policy')
        .many()
        .reply(200, JSON.stringify(
          {
            policy:
            {
              id: 'some-test-policy',
              name: 'All access policy',
              members: ['team:local:admins'],
              type: 'CHEF_MANAGED'
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .many()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V2',
            'minor': 'V0'
          }
        }));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/policies/some-test-policy');
    });

    describe('policy details', () => {

      it('displays heading', () => {
        waitForElement('chef-heading').then(heading => {
          expect(heading.getText()).toBe('All access policy');
        });
      });

      it('displays the id in the header', () => {
        waitForElement('header td:nth-child(1)').then(idHeader => {
          expect(idHeader.getText()).toBe('some-test-policy');
        });
      });

      it('displays the type in the header', () => {
        waitForElement('header td:nth-child(2)').then(typeHeader => {
          expect(typeHeader.getText()).toBe('Chef-managed');
        });
      });

      it('renders the json', () => {
        waitForElement('chef-snippet pre').then(policyJSON => {
          expect(policyJSON).not.toBeNull();
        });
      });
    });
  });

  // Note(sr): this is exploding in the console, but not failing the test.
  // Let's revisit this red herring, please! =)
  xdescribe('Policy members add', () => {
    beforeEach(() => {

      fakeServer()
        .get('/apis/iam/v2/policies/some-test-policy')
        .many()
        .reply(200, JSON.stringify(
          {
            policy:
            {
              id: 'some-test-policy',
              name: 'Test Policy',
              members: ['team:local:testteam', 'user:local:testuser'],
              type: 'CUSTOM'
            }
          }
        ));

      fakeServer()
        .get('/api/v0/auth/users')
        .many()
        .reply(200, JSON.stringify(
          {
            users: [
              {
                id: 'otheruser',
                name: 'Otheruser',
                email: 'otheruser',
                username: 'otheruser'
              },
              {
                id: 'admin',
                name: 'Local Administrator',
                email: 'admin',
                username: 'admin'
              },
              {
                id: 'testuser',
                name: 'Testuser',
                email: 'testuser',
                username: 'testuser'
              }
            ]
          }
        ));

      fakeServer()
        .get('/api/v0/auth/teams')
        .many()
        .reply(200, JSON.stringify(
          {
            teams: [
              {
                id: 'testteam',
                name: 'testteam',
                description: 'Testteam'
              },
              {
                id: 'admins',
                name: 'admins',
                description: 'admins'
              }
            ]
          }
        ));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/policies/some-test-policy/add-members');
    });

    it('displays heading', () => {
      const heading = $('chef-heading h1');
      expect(heading.getText()).toBe('Add Members to Test Policy');
    });

    it('displays the local teams and users that are not yet ' +
      'members in sorted order as links', () => {

        expect($('chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(2) a')
          .getText()).toBe('admin');
        expect($('chef-table-new chef-table-row:nth-child(1) chef-table-cell:nth-child(3)')
          .getText()).toBe('Local user');

        expect($('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(2) a')
          .getText()).toBe('admins');
        expect($('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(3)')
          .getText()).toBe('Local team');

        expect($('chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(2) a')
          .getText()).toBe('otheruser');
        expect($('chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(3)')
          .getText()).toBe('Local user');
      });

    describe('when a single member is added and submitted', () => {
      beforeEach(() => {
        fakeServer()
          .post('/apis/iam/v2/policies/some-test-policy/members:add',
            {
              'members': ['user:local:admin']
            })
          .many()
          .reply(200, JSON.stringify(
            {
              policy:
              {
                id: 'some-test-policy',
                name: 'Test Policy',
                members: ['team:local:admin', 'team:local:testteam', 'user:local:testuser'],
                type: 'CUSTOM'
              }
            }
          ));
      });

      it('it enables the add button, changes its text, and sends the request', () => {
        expect($('#right-buttons chef-button:first-child').getAttribute('disabled')).toBe('true');

        $('chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(1) chef-checkbox').click();
        expect($('#right-buttons chef-button:first-child').getText()).toBe('Add Member');
        expect($('#right-buttons chef-button:first-child').getAttribute('disabled')).toBeNull();

        $('#right-buttons chef-button:first-child').click();
      });
    });

    describe('when multiple members are selected and added', () => {
      beforeEach(() => {
        fakeServer()
          .post('/apis/iam/v2/policies/some-test-policy/members:add',
            {
              'members': ['user:local:admin', 'team:local:admins']
            })
          .many()
          .reply(200, JSON.stringify(
            {
              policy:
              {
                id: 'some-test-policy',
                name: 'Test Policy',
                members: ['team:local:admin', 'team:local:testteam', 'user:local:testuser'],
                type: 'CUSTOM'
              }
            }
          ));
      });

      it('it enables the add button, changes its text to indicate multiple members, ' +
        'and sends the request', () => {
          expect($('#right-buttons chef-button:first-child').getAttribute('disabled'))
            .toBe('true');

          $('chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(1) chef-checkbox')
            .click();
          $('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(1) chef-checkbox')
            .click();

          expect($('#right-buttons chef-button:first-child').getAttribute('disabled')).toBeNull();
          expect($('#right-buttons chef-button:first-child').getText()).toBe('Add 2 Members');

          $('#right-buttons chef-button:first-child').click();
        });
    });

    describe('when a custom expression is added and submitted' +
      'with incorrect expressions initially inputted', () => {

        beforeEach(() => {
          fakeServer()
            .post('/apis/iam/v2/policies/some-test-policy/members:add',
              {
                'members': ['user:ldap:*']
              })
            .many()
            .reply(200, JSON.stringify(
              {
                policy:
                {
                  id: 'some-test-policy',
                  name: 'Test Policy',
                  members: ['team:local:admin', 'team:local:testteam', 'user:local:testuser'],
                  type: 'CUSTOM'
                }
              }
            ));
        });

        it('shows errors for the incorrect inputs, adds and pre-selects the ' +
          'valid member to the table, and submits the request', () => {

            // Open modal
            $('#footer .add-member-button').click();

            // Button starts out disabled
            expect($('app-policy-add-members chef-modal chef-button:first-child')
              .getAttribute('disabled')).toBe('true');

            // Type an invalid expression, press submit and see error
            $('app-policy-add-members chef-modal chef-form-field input').sendKeys('something:invalid');
            expect($('app-policy-add-members chef-modal chef-button:first-child')
              .getAttribute('disabled')).toBeNull();
            $('app-policy-add-members chef-modal chef-button:first-child').click();
            expect($('app-policy-add-members chef-form-field .errors chef-error').getText())
              .toBe('Invalid member expression.');

            // When the member is already in the policy, you'll get an error
            $('app-policy-add-members chef-modal chef-form-field input').clear();
            $('app-policy-add-members chef-modal chef-form-field input')
              .sendKeys('team:local:testteam');
            expect($('app-policy-add-members chef-modal chef-button:first-child')
              .getAttribute('disabled')).toBeNull();
            $('app-policy-add-members chef-modal chef-button:first-child').click();
            expect($('app-policy-add-members chef-form-field .errors chef-error').getText())
              .toBe('Member already in policy.');

            // When the member is already in the table, you'll get an error
            $('app-policy-add-members chef-modal chef-form-field input').clear();
            $('app-policy-add-members chef-modal chef-form-field input').sendKeys('team:local:admins');
            expect($('app-policy-add-members chef-modal chef-button:first-child')
              .getAttribute('disabled')).toBeNull();
            $('app-policy-add-members chef-modal chef-button:first-child').click();
            expect($('app-policy-add-members chef-form-field .errors chef-error').getText())
              .toBe('Member expression already in table.');

            // Type a valid expression and submit
            $('app-policy-add-members chef-modal chef-form-field input').clear();
            $('app-policy-add-members chef-modal chef-form-field input').sendKeys('user:ldap:*');
            expect($('app-policy-add-members chef-modal chef-button:first-child')
              .getAttribute('disabled')).toBeNull();
            $('app-policy-add-members chef-modal chef-button:first-child').click();

            // Add members button should be enabled and should be added to table in sorted way
            expect($('#right-buttons chef-button:first-child').getText()).toBe('Add Member');

            expect($('chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(2) a').getText())
              .toBe('admin');
            expect($('chef-table-new chef-table-row:nth-child(1) chef-table-cell:nth-child(3)')
              .getText())
              .toBe('Local user');

            expect($('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(2) a').getText())
              .toBe('admins');
            expect($('chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(3)').getText())
              .toBe('Local team');

            // Non-local users / teams don't have a link and are pre-selected
            expect(
              $('chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(1) chef-checkbox')
                .getAttribute('ng-reflect-checked')).toBe('true');
            expect($('chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(2)')
              .getText()).toBe('All LDAP users');
            expect($('chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(3)')
              .getText()).toBe('All LDAP users');

            expect($('chef-table-new chef-table-body chef-table-row:nth-child(4) chef-table-cell:nth-child(2) a').getText())
              .toBe('otheruser');
            expect($('chef-table-new chef-table-body chef-table-row:nth-child(4) chef-table-cell:nth-child(3)').getText())
              .toBe('Local user');

            expect($('#right-buttons chef-button:first-child').getAttribute('disabled')).toBeNull();
            expect($('#right-buttons chef-button:first-child').getText()).toBe('Add Member');

            // Submit to API
            $('#right-buttons chef-button:first-child').click();
          });
      });
  });

  describe('Roles list', () => {
    beforeEach(() => {
      fakeServer()
        .get('/api/v0/auth/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/iam/v2/roles': {
                get: true,
                put: false,
                post: true,
                delete: false,
                patch: false
              }
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/roles')
        .many()
        .reply(200, JSON.stringify(
          {
            roles: [
              {
                id: '514c7547-e858-4b3c-a48f-2448320aeea5',
                name: 'Some role whose name does not start with A',
                actions: [],
                type: 'CUSTOM'
              },
              {
                id: 'c4a60965-95f7-44f3-b21d-0b27e478c0cc',
                name: 'Owner',
                actions: ['secrets:*'],
                type: 'CHEF_MANAGED'
              }
            ]
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .many()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V2',
            'minor': 'V0'
          }
        }));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/roles');
    });

    it('displays heading', () => {
      const heading = $('chef-heading');
      expect(heading.getText()).toBe('Roles');
    });

    it('displays role list component', () => {
      const rolesList = $('app-roles-list');
      expect(rolesList).not.toBeNull();
    });

    describe('displays the alphabetically sorted first role with name and type', () => {
      it('first role', () => {
        const name = $('app-roles-list chef-table-new chef-table-row:nth-child(1) chef-table-cell:first-child a');
        browser.wait(EC.textToBePresentInElement(name, 'Owner'));

        const policyType = $('chef-table-new chef-table-row:nth-child(1) chef-table-cell:nth-child(3)');
        browser.wait(EC.textToBePresentInElement(policyType, 'Chef-managed'));
      });

      it('second role', () => {
        const name = $('app-roles-list chef-table-new chef-table-row:nth-child(2) chef-table-cell:first-child a');
        browser.wait(EC.textToBePresentInElement(name,
          'Some role whose name does not start with A'));

        const policyType = $('chef-table-new chef-table-row:nth-child(2) chef-table-cell:nth-child(3)');
        browser.wait(EC.textToBePresentInElement(policyType, 'Custom'));
      });
    });
  });

  describe('Role details', () => {
    beforeEach(() => {
      fakeServer()
        .get('/apis/iam/v2/roles/some-test-role')
        .many()
        .reply(200, JSON.stringify(
          {
            role: {
              id: 'some-test-role',
              name: 'Owner',
              actions: ['secrets:*', 'other:*'],
              type: 'CHEF_MANAGED'
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .many()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V2',
            'minor': 'V0'
          }
        }));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/roles/some-test-role');
    });

    describe('Role details', () => {

      it('displays heading', () => {
        const heading = $('chef-heading');
        expect(heading.getText()).toBe('Owner');
      });

      it('displays the id in the header', () => {
        const idHeader = $('header td:nth-child(1)');
        expect(idHeader.getText()).toBe('some-test-role');
      });

      it('displays the type in the header', () => {
        const typeHeader = $('header td:nth-child(2)');
        expect(typeHeader.getText()).toBe('Chef-managed');
      });

      it('renders the json', () => {
        const rolesList = $('chef-snippet pre');
        expect(rolesList).not.toBeNull();
      });
    });
  });

  describe('Projects list', () => {
    beforeEach(() => {
      fakeServer()
        .get('/api/v0/auth/introspect')
        .any()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/iam/v2/projects': {
                get: true,
                put: false,
                post: true,
                delete: false,
                patch: false
              }
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/projects')
        .any()
        .reply(200, JSON.stringify(
          {
            projects: [
              {
                id: 'project-9',
                name: 'Some project whose name does not start with A',
                type: 'CUSTOM'
              },
              {
                id: 'default',
                name: 'Default Project',
                type: 'CHEF_MANAGED'
              },
              {
                id: 'project-19',
                name: 'This is custom, and authz allows deletion',
                type: 'CUSTOM'
              }
            ]
          }
        ));

      // mock up app-authorized responses
      [
        ['project-9', false],
        ['default', false],
        ['project-19', true]
      ].forEach(([id, deletable]) => {
        const path = `/iam/v2/projects/${id}`;
        const endpoints = {
          [path]: {
            get: true,
            put: false,
            post: true,
            delete: deletable,
            patch: false
          }
        };
        fakeServer()
          .post('/api/v0/auth/introspect', JSON.stringify(
            {
              path,
              parameters: []
            }
          ))
          .any()
          .reply(200, JSON.stringify({ endpoints }));
      });

      fakeServer()
        .get('/apis/iam/v2/policy_version')
        .any()
        .reply(200, JSON.stringify({
          'version': {
            'major': 'V2',
            'minor': 'V1'
          }
        }));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/projects');
    });

    it('displays heading', () => {
      const heading = $('chef-heading');
      browser.wait(EC.visibilityOf(heading), 5000, 'chef-heading should render');
      expect(heading.getText()).toBe('Projects');
    });

    it('displays project list component', () => {
      const projectsList = $('app-project-list');
      browser.wait(EC.visibilityOf(projectsList), 5000, 'project list should render');
      expect(projectsList).not.toBeNull();
    });

    describe('displays the first project (cannot be deleted and is chef-managed)', () => {
      it('shows name and id', () => {
        const name = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'first project should render');
        expect(name.getText()).toBe('Default Project');
        expect(name.getAttribute('href')).toMatch(/\/settings\/projects\/default$/);

        const projectID = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(1) chef-table-cell:nth-child(2)');
        expect(projectID.getText()).toBe('default');
      });

      it('does not show the control button', () => {
        const controlButton = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(1) ' +
          'chef-table-cell:nth-child(5) chef-control-menu');
        expect(controlButton.isPresent()).toEqual(false);
      });
    });

    describe('displays the second project (cannot be deleted, custom)', () => {
      it('shows name and id', () => {
        const name = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'second project should render');
        expect(name.getText()).toBe('Some project whose name does not start with A');
        expect(name.getAttribute('href')).toMatch(/\/settings\/projects\/project-9$/);

        const projectID = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(2) chef-table-cell:nth-child(2)');
        expect(projectID.getText()).toBe('project-9');
      });

      it('does not show the control button', () => {
        const controlButton = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(2) ' +
          'chef-table-cell:nth-child(5) chef-control-menu');
        expect(controlButton.isPresent()).toEqual(false);
      });
    });

    describe('displays the third project (can be deleted, custom)', () => {
      it('shows name and id', () => {
        const name = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'third project should render');
        expect(name.getText()).toBe('This is custom, and authz allows deletion');

        const projectID = $(
          'app-project-list chef-table-new chef-table-body chef-table-row:nth-child(3) chef-table-cell:nth-child(2)');
        expect(projectID.getText()).toBe('project-19');
      });

      it('shows the control button', () => {
        waitForElement('app-project-list chef-table-new chef-table-body chef-table-row:nth-child(3) ' +
          'chef-table-cell:nth-child(4) chef-control-menu').then(controlButton => {
            expect(controlButton.isPresent()).toBeTruthy();
            ['Delete Project'].forEach((item, index) => {
              it(`when clicked, shows ${item}`, () => {
                $('app-project-list chef-table-new chef-table-body chef-table-row:nth-child(2) ' +
                  'chef-table-cell:nth-child(3) chef-control-menu').
                  click();
                const dropDownOption = $(
                  `app-project-list chef-table-new chef-table-body chef-table-row:nth-child(2)
                  chef-table-cell:nth-child(3) chef-control-menu chef-option:nth-child(${index + 1})`);
                const dropDownOpened = () => dropDownOption.getText().then(val => val === item);
                browser.wait(dropDownOpened, 5000, 'Control options should render.');
              });
            });
          });
      });

      it('after delete is clicked and if the backend call succeeds, ' +
        'project is removed from list', () => {
          fakeServer()
            .delete('/apis/iam/v2/projects/project-19')
            .min(1).max(1)
            .reply(200);

          waitForElement('app-project-list chef-table-new chef-table-body chef-table-row:nth-child(3)')
            .then(third => {
              browser.wait(EC.presenceOf(third.$('chef-control-menu'))).then(() => {
                const controlButton = third.$('chef-control-menu');
                browser.wait(EC.elementToBeClickable(controlButton));
                controlButton.click().then(() => {
                  // select Delete Project
                  const deleteOption = controlButton.$('chef-option:nth-child(1)');
                  browser.wait(EC.visibilityOf(deleteOption), 5000,
                    'Delete option should render');
                  deleteOption.click();

                  // confirm Delete in modal
                  const deleteModal = $('app-delete-object-modal');
                  browser.wait(EC.presenceOf(deleteModal), 5000,
                    'Delete confirm modal should appear');

                  // confirm
                  deleteModal.$('chef-button:first-child button').click();

                  browser.wait(EC.not(EC.presenceOf(third)), 5000, 'third row should disappear');
                });
              });
            });
        });
    });
  });

  describe('Project details (custom)', () => {
    beforeEach(() => {
      fakeServer()
        .get('/apis/iam/v2/projects/my-project')
        .any()
        .reply(200, JSON.stringify(
          {
            project: {
              id: 'my-project',
              name: 'My Project',
              type: 'CUSTOM'
            }
          }
        ));

      fakeServer()
        .get('/apis/iam/v2/projects/my-project/rules')
        .any()
        .reply(200, JSON.stringify(
          {
            rules: [],
            status: 'NO_RULES'
          }
        ));

      browser.waitForAngularEnabled(false);
      browser.get('/settings/projects/my-project');
    });

    describe('project details', () => {
      it('displays heading', () => {
        waitForElement('chef-heading').then(heading => {
          expect(heading.getText()).toBe('My Project');
        });
      });

      it('displays the id in the header', () => {
        waitForElement('header td:nth-child(1)').then(idHeader => {
          expect(idHeader.getText()).toBe('my-project');
        });
      });

      describe('before any typing occurs', () => {
        it('displays the project name in the input and the save button is disabled', () => {
          waitForSpinners().then(() => {
            const detailsLink = $('#chef-option2');
            detailsLink.click().then(() => {
              const projectNameInput = $('app-project-details section form chef-form-field input');
              expect(projectNameInput.getAttribute('value')).toBe('My Project');

              const projectSaveButton = $('app-project-details section #button-bar button');
              expect(projectSaveButton.getAttribute('disabled')).toBe('true');
            });
          });
        });
      });

      describe('when the input value is changed to something new and saved', () => {
        beforeEach(() => {
          fakeServer()
            .put('/apis/iam/v2/projects/my-project',
              JSON.stringify({ name: 'My Project Changed' }))
            .many()
            .reply(200, JSON.stringify(
              {
                project: {
                  id: 'my-project',
                  name: 'My Project Changed',
                  type: 'CUSTOM'
                }
              }
            ));
          browser.waitForAngularEnabled(false);
          browser.get('/settings/projects/my-project');
        });

        it('enables the save button, updates the project, and notes the save, ' +
          'and then removes note once more typing occurs', () => {
            waitForSpinners().then(() => {
              const detailsLink = $('#chef-option2');
              detailsLink.click().then(() => {
                const projectSaveButton = $('app-project-details section #button-bar button');
                const projectNameInput = $(
                  'app-project-details section form chef-form-field input');
                expect(projectSaveButton.getAttribute('disabled')).toBe('true');
                browser.wait(EC.elementToBeClickable(projectNameInput), 5000).then(() => {
                  expect(projectNameInput.getAttribute('disabled')).toBeNull();

                  projectNameInput.sendKeys(' Changed');
                  expect(projectNameInput.getAttribute('value')).toBe('My Project Changed');

                  expect(projectSaveButton.getAttribute('disabled')).toBeNull();

                  // Save the project
                  $('app-project-details section #button-bar chef-button').click();

                  const heading = $('chef-heading');
                  browser.wait(EC.textToBePresentInElement(heading, 'My Project Changed'), 5000);
                  expect(heading.getText()).toBe('My Project Changed');
                  expect(projectSaveButton.getAttribute('disabled')).toBe('true');
                  expect($('app-project-details section #button-bar #saved-note').getText())
                    .toBe('All changes saved.');

                  browser.wait(EC.elementToBeClickable(projectNameInput), 5000);
                  // Type once more
                  projectNameInput.sendKeys(' Once More');
                  expect(projectSaveButton.getAttribute('disabled')).toBeNull();

                  // Removed save note
                  expect($('app-project-details section #button-bar #saved-note').
                    isPresent()).toBeFalsy();
                });
              });
            });
          });
      });
    });
  });
});
