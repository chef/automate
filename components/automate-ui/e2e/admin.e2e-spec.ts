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
        .get('/apis/iam/v2/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/apis/iam/v2/users': {
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
        .get('/apis/iam/v2/users')
        .many()
        .reply(200, JSON.stringify(
          {
            users: [
              {
                id: 'admin',
                name: 'Local Administrator',
                membership_id: '38d792f7-85b4-4127-9c4e-110118e3cca4'
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
      expectCountElements('chef-table chef-thead chef-tr', 1);
      // one row for the admin user in the table body
      expectCountElements('chef-table chef-tbody chef-tr', 1);
    });

    it('displays create user button', () => {
      const addButton = $('.page-body chef-button');
      browser.wait(EC.textToBePresentInElement(addButton, 'Create User'), 5000);
    });

    it('displays the returned user info', () => {
      const fullname = $('chef-table chef-tbody chef-tr chef-td:first-child a');
      browser.wait(EC.textToBePresentInElement(fullname, 'Local Administrator'), 5000);

      const username = $('chef-table chef-tbody chef-tr chef-td:nth-child(2)');
      browser.wait(EC.textToBePresentInElement(username, 'admin'), 5000);
    });

    describe('control button', () => {
      it('is displayed', () => {
        waitForElement('chef-table chef-tbody chef-tr mat-select')
        .then((e) => {
          expect(e.isDisplayed()).toBeTruthy();
        });
      });

      ['Delete User'].forEach((item, index) => {
        it(`when clicked, shows ${item}`, () => {
          const menuTrigger = $('chef-table chef-tbody chef-tr mat-select .mat-select-trigger');
          browser.wait(EC.elementToBeClickable(menuTrigger), 5000).then(() => {
            menuTrigger.click().then(() => {
              const dropDownOption = $(`.chef-control-menu mat-option:nth-child(${index + 1}`);
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
        .get('/apis/iam/v2/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/apis/iam/v2/policies': {
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
                type: 'CUSTOM',
                statements: [
                  { effect: 'ALLOW', role: 'some-role', resources: ['*'], projects: [] }
                ],
                projects: []
              },
              {
                id: 'chef-managed-administrator',
                name: 'Administrator All Access',
                members: ['team:local:admins'],
                statements: [
                  { effect: 'ALLOW', actions: ['*'], resources: ['*'], projects: [] }
                ],
                type: 'CHEF_MANAGED',
                projects: []
              }
            ]
          }
        ));

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
        waitForElement('app-policy-list chef-table chef-tr:nth-child(1) chef-td:first-child a')
          .then((name) => {
            expect(name.getText()).toBe('Administrator All Access');
          });

        waitForElement('chef-table chef-tr:nth-child(1) chef-td:nth-child(2)')
          .then((id) => {
            expect(id.getText()).toBe('chef-managed-administrator');
          });

        waitForElement('chef-table chef-tr:nth-child(1) chef-td:nth-child(3)')
          .then((policyType) => {
            expect(policyType.getText()).toBe('Chef-managed');
          });

        waitForElement('app-policy-list chef-table chef-tr:nth-child(1) chef-td:nth-child(4)')
          .then((members) => {
            expect(members.getText()).toBe('In use');
          });
      });

      it('second policy', () => {
        waitForElement('app-policy-list chef-table chef-tr:nth-child(2) chef-td:first-child a')
          .then(name => {
            expect(name.getText()).toBe('Some policy whose name does not start with A');
          });

        waitForElement('chef-table chef-tr:nth-child(2) chef-td:nth-child(2)')
          .then((id) => {
            expect(id.getText()).toBe('some-policy-id');
          });

        waitForElement('chef-table chef-tr:nth-child(2) chef-td:nth-child(3)')
        .then(policyType => {
          expect(policyType.getText()).toBe('Custom');
        });

        waitForElement('app-policy-list chef-table chef-tr:nth-child(2) chef-td:nth-child(4)')
          .then(members => {
            expect(members.getText()).toBe('No members');
          });
      });
    });

    describe('control menu', () => {
      it('is not displayed for chef-managed policies', () => {
        // admin policy row
        expect(
          $('chef-table chef-tbody chef-tr:nth-child(1) mat-select')
            .isPresent()).toBeFalsy();
      });

      it('is displayed for custom policies', () => {
        // custom policy row
        waitForElement('chef-table chef-tbody chef-tr:nth-child(2) mat-select')
          .then(controlButton => {
            expect(controlButton.isPresent()).toBeTruthy();
          });
      });

      ['Delete Policy'].forEach((item, index) => {
        it(`when clicked, shows ${item}`, () => {
          waitForElement(
            'chef-table chef-tbody chef-tr:nth-child(2) mat-select .mat-select-trigger')
            .then(controlButton => {
              browser.wait(EC.elementToBeClickable(controlButton));
              controlButton.click().then(() => {
                const dropDownOption = $(`.chef-control-menu mat-option:nth-child(${index + 1}`);
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

        waitForElement('chef-table chef-tbody chef-tr:nth-child(2)')
        .then(somePolicy => {
          // open control menu
          const controlButton = somePolicy.$('mat-select .mat-select-trigger');
          browser.wait(EC.elementToBeClickable(controlButton));
          controlButton.click().then(() => {
            // select Delete Policy
            const deleteOption = $('.chef-control-menu mat-option:nth-child(1)');
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
              type: 'CHEF_MANAGED',
              statements: [
                { effect: 'ALLOW', role: 'some-role', resources: ['*'], projects: [] }
              ],
              projects: []
            }
          }
        ));

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

  describe('Roles list', () => {
    beforeEach(() => {
      fakeServer()
        .get('/apis/iam/v2/introspect')
        .many()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/apis/iam/v2/roles': {
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
        const name = $('app-roles-list chef-table chef-tr:nth-child(1) chef-td:first-child a');
        browser.wait(EC.textToBePresentInElement(name, 'Owner'));

        const policyType = $('chef-table chef-tr:nth-child(1) chef-td:nth-child(3)');
        browser.wait(EC.textToBePresentInElement(policyType, 'Chef-managed'));
      });

      it('second role', () => {
        const name = $('app-roles-list chef-table chef-tr:nth-child(2) chef-td:first-child a');
        browser.wait(EC.textToBePresentInElement(name,
          'Some role whose name does not start with A'));

        const policyType = $('chef-table chef-tr:nth-child(2) chef-td:nth-child(3)');
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
        .get('/apis/iam/v2/introspect')
        .any()
        .reply(200, JSON.stringify(
          {
            endpoints: {
              '/apis/iam/v2/projects': {
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
        const path = `/apis/iam/v2/projects/${id}`;
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
          .post('/apis/iam/v2/introspect', JSON.stringify(
            {
              path,
              parameters: []
            }
          ))
          .any()
          .reply(200, JSON.stringify({ endpoints }));
      });

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
          'app-project-list chef-table chef-tbody chef-tr:nth-child(1) chef-td:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'first project should render');
        expect(name.getText()).toBe('Default Project');
        expect(name.getAttribute('href')).toMatch(/\/settings\/projects\/default$/);

        const projectID = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(1) chef-td:nth-child(2)');
        expect(projectID.getText()).toBe('default');
      });

      it('does not show the control button', () => {
        const controlButton = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(1) ' +
          'chef-td:nth-child(5) mat-select');
        expect(controlButton.isPresent()).toEqual(false);
      });
    });

    describe('displays the second project (cannot be deleted, custom)', () => {
      it('shows name and id', () => {
        const name = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(2) chef-td:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'second project should render');
        expect(name.getText()).toBe('Some project whose name does not start with A');
        expect(name.getAttribute('href')).toMatch(/\/settings\/projects\/project-9$/);

        const projectID = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(2) chef-td:nth-child(2)');
        expect(projectID.getText()).toBe('project-9');
      });

      it('does not show the control button', () => {
        const controlButton = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(2) ' +
          'chef-td:nth-child(5) mat-select');
        expect(controlButton.isPresent()).toEqual(false);
      });
    });

    describe('displays the third project (can be deleted, custom)', () => {
      it('shows name and id', () => {
        const name = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(3) chef-td:nth-child(1) a');
        browser.wait(EC.visibilityOf(name), 5000, 'third project should render');
        expect(name.getText()).toBe('This is custom, and authz allows deletion');

        const projectID = $(
          'app-project-list chef-table chef-tbody chef-tr:nth-child(3) chef-td:nth-child(2)');
        expect(projectID.getText()).toBe('project-19');
      });

      it('shows the control button', () => {
        waitForElement('app-project-list chef-table chef-tbody chef-tr:nth-child(3) ' +
          'chef-td:nth-child(4) mat-select').then(controlButton => {
            expect(controlButton.isPresent()).toBeTruthy();
            ['Delete Project'].forEach((item, index) => {
              it(`when clicked, shows ${item}`, () => {
                $('app-project-list chef-table chef-tbody chef-tr:nth-child(2) ' +
                  'chef-td:nth-child(3) mat-select .mat-select-trigger').
                  click();
                const dropDownOption = $(`.chef-control-menu mat-option:nth-child(${index + 1})`);
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

          waitForElement('app-project-list chef-table chef-tbody chef-tr:nth-child(3)')
            .then(third => {
              browser.wait(EC.presenceOf(third.$('mat-select'))).then(() => {
                const controlButton = third.$('mat-select .mat-select-trigger');

                browser.wait(EC.elementToBeClickable(controlButton));
                controlButton.click().then(() => {
                  // select Delete Project
                  const deleteOption = $('.chef-control-menu mat-option:nth-child(1)');
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
