import { $, $$, browser, by, element } from 'protractor';
import { fakeServer } from './helpers/fake_server';

describe('Integrations', () => {

  beforeAll(() => {
    browser.waitForAngularEnabled(false);

    // Enable beta node managers
    $('body').sendKeys('feat');
    element(by.cssContainingText('.feature-flags .title', 'Azure Node Manager')).click();
    element(by.cssContainingText('.feature-flags .title', 'Google Cloud Node Manager')).click();
  });

  describe('integrations view', () => {
    beforeEach(() => {
      fakeServer()
        .post('/api/v0/nodemanagers/search', '{}')
        .any()
        .reply(200, JSON.stringify({
          managers: [
            {
              id: 'e69dc612-7e67-43f2-9b19-256afd385820',
              name: 'Automate',
              type: 'automate',
              credential_id: '',
              instance_credentials: [],
              status: '',
              account_id: '',
              date_added: '2018-11-19T04:37:13Z',
              credential_data: []
            },
            {
              id: 'f69dc612-7e67-43f2-9b19-256afd385820',
              name: 'aws-manager-1',
              type: 'aws-api',
              credential_id: '',
              instance_credentials: [],
              status: '',
              account_id: '',
              date_added: '2018-11-19T04:37:13Z',
              credential_data: []
            },
            {
              id: 'f69dc612-7e67-43f2-9b19-256afd385821',
              name: 'aws-manager-2',
              type: 'aws-api',
              credential_id: '',
              instance_credentials: [],
              status: '',
              account_id: '',
              date_added: '2018-11-19T04:52:13Z',
              credential_data: []
            }
          ],
          total: 1
        }));
    });

    it('has a heading and a subheading', () => {
      browser.get('/settings/node-integrations');

      const heading = $('chef-page-header chef-heading');
      const subheading = $('chef-page-header chef-subheading');

      expect(heading.getText())
        .toBe('Node Integrations');
      expect(subheading.getText())
        .toBe('Connect Chef Automate to cloud services.');
    });

    it('displays list of integrations', () => {
      browser.get('/settings/node-integrations');

      const list = $('chef-table');
      const nameColumn = list.$$('chef-tbody chef-tr:not(.new-row)').map(row => {
        return row.$('chef-td:first-child').getText();
      });

      expect(list.isDisplayed()).toEqual(true);
      expect(nameColumn).toEqual([
        'aws-manager-1',
        'aws-manager-2'
      ]);
    });

    describe('integrations list item', () => {
      it('has a link to detail view', () => {
        browser.get('/settings/node-integrations');

        const listItem = $$('chef-tbody chef-tr').first();
        const link = listItem.element(by.css('.link'));

        expect(link.isDisplayed()).toEqual(true);
        expect(link.getText()).toEqual('aws-manager-1');
        expect(link.getAttribute('href'))
          .toContain('/settings/node-integrations/f69dc612-7e67-43f2-9b19-256afd385820');
      });

      it('has control menu (delete button)', () => {
        browser.get('/settings/node-integrations');

        const listItem = $$('chef-tbody chef-tr:not(.new-row)').filter(row => {
          return row.$('chef-td:first-child').getText().then(text => {
            return text !== 'Automate';
          });
        }).first();
        const controlMenu = listItem.element(by.tagName('chef-control-menu'));

        expect(controlMenu.isDisplayed()).toBe(true);
      });
    });
  });

  describe('integrations-add view', () => {
    it('has a heading and a subheading', () => {
      browser.get('/settings/node-integrations/add');

      const heading = $('chef-page-header chef-heading');
      const subheading = $('chef-page-header chef-subheading');

      expect(heading.getText())
        .toBe('Add a new cloud management service');
      expect(subheading.getText())
        .toBe('Select a cloud management service and add your credentials for the account.');
    });

    it('has a `cancel` button', () => {
      browser.get('/settings/node-integrations/add');

      const button = element(by.cssContainingText('chef-button', 'Cancel'));

      expect(button.isDisplayed()).toEqual(true);
    });

    describe('clicking `cancel` button', () => {
      it('navigates back to credential-list view', () => {
        browser.get('/settings/node-integrations/add');

        const button = element(by.cssContainingText('chef-button', 'Cancel'));

        button.click();

        expect(browser.getCurrentUrl()).toMatch(/\/settings\/node-integrations$/);
      });
    });

    it('has options for selecting an integration type', () => {
      browser.get('/settings/node-integrations/add');

      const label = element(by.cssContainingText('.label', 'Select a node management service'));
      const options = $$('.integration-services chef-option');

      expect(label.isDisplayed()).toEqual(true);
      expect(options.map(el => el.getAttribute('value'))).toEqual([
        'aws',
        'azure',
        'gcp'
      ]);
    });

    describe('gcp form', () => {
      beforeEach(() => {
        browser.get('/settings/node-integrations/add');

        const option = $('chef-option[value="gcp"]');

        option.click();
      });

      it('has a name input', () => {
        const label = element(by.cssContainingText('.label', 'Name'));
        const input = $('chef-input[formcontrolname="name"]');

        expect(label.isDisplayed()).toEqual(true);
        expect(input.isDisplayed()).toEqual(true);
      });

      it('has a textarea to enter credentials JSON', () => {
        const label = element(
          by.cssContainingText('.label', 'Enter your service account JSON credentials')
        );
        const textarea = $('textarea[formcontrolname="google_credentials_json"]');

        expect(label.isDisplayed()).toEqual(true);
        expect(textarea.isDisplayed()).toEqual(true);
      });
    });
  });

  describe('integrations-edit view', () => {
    beforeEach(() => {
      fakeServer()
        .get('/api/v0/nodemanagers/id/be56f1e3-4ac9-4e74-8d69-f67bea007cd3')
        .many()
        .reply(200, JSON.stringify({
          id: 'be56f1e3-4ac9-4e74-8d69-f67bea007cd3',
          name: 'gcp-test-manager',
          type: 'gcp-api',
          credential_id: 'e68d4cb7-5a1c-41cf-870e-6af7b2a15e5c',
          instance_credentials: [],
          status: 'reachable',
          account_id: 'alex-pop-project',
          date_added: '2018-10-16T04:10:51Z',
          credential_data: []
        }));

      browser.get('/settings/node-integrations/edit/be56f1e3-4ac9-4e74-8d69-f67bea007cd3');
    });

    it('has a heading and a subheading', () => {
      const heading = $('chef-page-header chef-heading');
      const subheading = $('chef-page-header chef-subheading');

      expect(heading.getText())
        .toBe('Update cloud management service');
      expect(subheading.getText())
        .toBe('Select a cloud management service and add your credentials for the account.');
    });

    it('has a `cancel` button', () => {
      const button = element(by.cssContainingText('chef-button', 'Cancel'));

      expect(button.isDisplayed()).toEqual(true);
    });

    describe('clicking `cancel` button', () => {
      it('navigates back to credential-list view', () => {
        const button = element(by.cssContainingText('chef-button', 'Cancel'));

        button.click();

        expect(browser.getCurrentUrl()).toMatch(/\/settings\/node-integrations$/);
      });
    });

    it('has options for selecting an integration type', () => {
      const label = element(by.cssContainingText('.label', 'Select a node management service'));
      const options = $$('.integration-services chef-option');

      expect(label.isDisplayed()).toEqual(true);
      expect(options.map(el => el.getAttribute('value'))).toEqual([
        'aws',
        'azure',
        'gcp'
      ]);
    });

    // Note(sr) 2018-11-19: this seems to be flakey. Disabled until someone has
    // a look.
    xit('has the correct integration type option selected', () => {
      const selected = $('.integration-services chef-option.selected');

      expect(selected.getAttribute('value')).toEqual('gcp');
    });

    describe('gcp form', () => {
      it('has a name input filled with correct value', () => {
        const label = element(by.cssContainingText('.label', 'Name'));
        const input = $('chef-input[formcontrolname="name"]');

        expect(label.isDisplayed()).toEqual(true);
        expect(input.isDisplayed()).toEqual(true);
        expect(input.getAttribute('value')).toEqual('gcp-test-manager');
      });

      it('has an empty textarea to update credentials JSON', () => {
        const label = element(
          by.cssContainingText('.label', 'Enter your service account JSON credentials')
        );
        const textarea = $('textarea[formcontrolname="google_credentials_json"]');

        expect(label.isDisplayed()).toEqual(true);
        expect(textarea.isDisplayed()).toEqual(true);
        expect(textarea.getText()).toEqual('');
      });
    });
  });

  describe('integrations-detail view', () => {
    beforeEach(() => {
      // Request for nodemanager info
      fakeServer()
        .get('/api/v0/nodemanagers/id/be56f1e3-4ac9-4e74-8d69-f67bea007cd3')
        .many()
        .reply(200, JSON.stringify({
          id: 'be56f1e3-4ac9-4e74-8d69-f67bea007cd3',
          name: 'gcp-test-manager',
          type: 'gcp-api',
          credential_id: 'e68d4cb7-5a1c-41cf-870e-6af7b2a15e5c',
          instance_credentials: [],
          status: 'reachable',
          account_id: 'alex-pop-project',
          date_added: '2018-10-16T04:10:51Z',
          credential_data: []
        }));

      // Request for list of nodes belonging to nodemanager
      fakeServer()
        .post('/api/v0/nodes/search', JSON.stringify({
          'filters': [
            {
              'key': 'manager_id',
              'values': ['be56f1e3-4ac9-4e74-8d69-f67bea007cd3']
            }
          ],
          'page': 1,
          'per_page': 100
        }))
        .many()
        .reply(200, JSON.stringify({
          'nodes': [
            {
              'id': '659e200d-35d2-4146-b98a-50e969855f2a',
              'name': 'test-node-1',
              'platform': 'ubuntu',
              'platform_version': '18.04',
              'manager': 'gcp-api',
              'tags': [],
              'last_contact': '2019-03-12T06:14:36Z',
              'status': 'reachable',
              'last_job': null,
              'target_config': null,
              'manager_ids': ['be56f1e3-4ac9-4e74-8d69-f67bea007cd3'],
              'state': 'RUNNING',
              'name_prefix': '',
              'projects': []
            },
            {
              'id': '659e200d-35d2-4146-b98a-50e969855f2b',
              'name': 'test-node-2',
              'platform': 'ubuntu',
              'platform_version': '18.04',
              'manager': 'gcp-api',
              'tags': [],
              'last_contact': '2019-03-12T06:14:36Z',
              'status': 'unreachable',
              'last_job': null,
              'target_config': null,
              'manager_ids': ['be56f1e3-4ac9-4e74-8d69-f67bea007cd3'],
              'state': 'RUNNING',
              'name_prefix': '',
              'projects': []
            }
          ],
          'total': 2,
          'total_unreachable': 1,
          'total_reachable': 1,
          'total_unknown': 0
        }));

      browser.get('/settings/node-integrations/be56f1e3-4ac9-4e74-8d69-f67bea007cd3');
    });

    it('displays a heading with the manager name', () => {
      const heading = $('chef-page-header chef-heading');

      expect(heading.getText()).toEqual('gcp-test-manager');
    });

    it('displays a manager type', () => {
      const label = $('#manager-info th:nth-child(1)');
      const value = $('#manager-info td:nth-child(1)');

      expect(label.getText()).toEqual('Manager');
      expect(value.getText()).toEqual('gcp-api');
    });

    it('displays a manager status', () => {
      const label = $('#manager-info th:nth-child(2)');
      const value = $('#manager-info td:nth-child(2)');

      expect(label.getText()).toEqual('Status');
      expect(value.getText()).toEqual('reachable');
    });

    it('displays a manager node count', () => {
      const label = $('#manager-info th:nth-child(3)');
      const value = $('#manager-info td:nth-child(3)');

      expect(label.getText()).toEqual('Node Count');
      expect(value.getText()).toEqual('2');
    });

    it('displays a list of nodes belonging to the nodemanager', () => {
      const list = $('#manager-nodes-list');
      const nameColumn = list.$$('chef-tbody chef-tr').map(row => {
        return row.$('chef-td:nth-child(2)').getText();
      });

      expect(list.isDisplayed()).toEqual(true);
      expect(nameColumn).toEqual([
        'test-node-1',
        'test-node-2'
      ]);
    });
  });
});
