import { $, $$, browser, by, element, ExpectedConditions as EC } from 'protractor';
import { fakeServer } from './helpers/fake_server';

describe('Job Add', () => {

  beforeEach(() => {
    browser.waitForAngularEnabled(false);

    fakeServer()
      .post('/api/v0/nodemanagers/search', '{}')
      .reply(200, JSON.stringify({
        managers: [
          {
            account_id: '',
            credential_data: [],
            credential_id: '5f9e543e-e263-454f-ab8f-b6b0c1d984a1',
            date_added: '2018-10-24T04:31:20Z',
            id: '7880e825-ea1c-42df-9876-8f9413dddda4',
            instance_credentials: [],
            name: 'aws-api-1',
            status: 'reachable',
            type: 'aws-api'
          },
          {
            account_id: '',
            credential_data: [],
            credential_id: '086e9fa9-e83c-4714-a65d-ff362990af45',
            date_added: '2018-10-24T04:20:33Z',
            id: '12741bc4-2afc-46b8-b7d7-becd8552938c',
            instance_credentials: [],
            name: 'aws-ec2-1',
            status: 'reachable',
            type: 'aws-ec2'
          },
          {
            account_id: '',
            credential_data: [],
            credential_id: 'b3f0fdbf-cd3e-49f6-844d-f8db2bba2bd9',
            date_added: '2018-10-24T04:32:59Z',
            id: '433b73e3-a960-4469-a111-932dc661ec02',
            instance_credentials: [],
            name: 'azure-api-1',
            status: 'reachable',
            type: 'azure-api'
          },
          {
            account_id: '',
            credential_data: [],
            credential_id: '265ecde7-7a87-45ca-9168-b1652158bb6a',
            date_added: '2018-10-24T04:34:05Z',
            id: 'd62908d6-0b3a-478f-88c9-8be43e07bda5',
            instance_credentials: [],
            name: 'azure-vm-1',
            status: 'reachable',
            type: 'azure-vm'
          },
          {
            account_id: '',
            credential_data: [],
            credential_id: 'e68d4cb7-5a1c-41cf-870e-6af7b2a15e5c',
            date_added: '2018-10-16T04:10:51Z',
            id: 'be56f1e3-4ac9-4e74-8d69-f67bea007cd3',
            instance_credentials: [],
            name: 'gcp-test-manager',
            status: 'reachable',
            type: 'gcp-api'
          },
          {
            account_id: '',
            credential_data: [],
            credential_id: '',
            date_added: '2018-09-21T03:32:41Z',
            id: 'e69dc612-7e67-43f2-9b19-256afd385820',
            instance_credentials: [],
            name: 'Automate',
            status: '',
            type: 'automate'
          }
        ],
        total: 6
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/7880e825-ea1c-42df-9876-8f9413dddda4/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'aws-api-node-1',
          'aws-api-node-2',
          'aws-api-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/12741bc4-2afc-46b8-b7d7-becd8552938c/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'aws-ec2-node-1',
          'aws-ec2-node-2',
          'aws-ec2-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/d62908d6-0b3a-478f-88c9-8be43e07bda5/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'azure-vm-node-1',
          'azure-vm-node-2',
          'azure-vm-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/433b73e3-a960-4469-a111-932dc661ec02/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'azure-api-node-1',
          'azure-api-node-2',
          'azure-api-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/be56f1e3-4ac9-4e74-8d69-f67bea007cd3/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'gcp-api-node-1',
          'gcp-api-node-2',
          'gcp-api-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/e69dc612-7e67-43f2-9b19-256afd385820/search-nodes',
        JSON.stringify({query: {filter_map: []}}))
      .any()
      .reply(200, JSON.stringify({
        nodes: [
          'automate-node-1',
          'automate-node-2',
          'automate-node-3'
        ],
        total: 3
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/7880e825-ea1c-42df-9876-8f9413dddda4/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'regions'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/7880e825-ea1c-42df-9876-8f9413dddda4/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'tags'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/12741bc4-2afc-46b8-b7d7-becd8552938c/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'regions'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/12741bc4-2afc-46b8-b7d7-becd8552938c/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'tags'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/433b73e3-a960-4469-a111-932dc661ec02/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'regions'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/433b73e3-a960-4469-a111-932dc661ec02/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'tags'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/d62908d6-0b3a-478f-88c9-8be43e07bda5/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'regions'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/d62908d6-0b3a-478f-88c9-8be43e07bda5/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'tags'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/e69dc612-7e67-43f2-9b19-256afd385820/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'name'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: [
          'automate-node-1'
        ]
      }));

    fakeServer()
      .post('/api/v0/nodemanagers/id/e69dc612-7e67-43f2-9b19-256afd385820/search-fields',
        JSON.stringify({query: {filter_map: []}, field: 'tags'})
      )
      .any()
      .reply(200, JSON.stringify({
        fields: []
      }));

    fakeServer()
      .post('/api/v0/compliance/profiles/search', JSON.stringify({
        owner: 'testchefuser'
      }))
      .reply(200, JSON.stringify({
        profiles: [
          {
            name: 'apache-baseline',
            title: 'DevSec Apache Baseline',
            maintainer: 'DevSec Hardening Framework Team',
            copyright: 'DevSec Hardening Framework Team',
            copyright_email: 'hello@dev-sec.io',
            license: 'Apache 2 license',
            summary: 'Test-suite for best-practice apache hardening',
            version: '2.0.2',
            owner: 'admin',
            supports: [],
            depends: [],
            sha256: '3e1310b071dc4d706263e9d07083e10a92b4b69e4a36cffa1eda7eaecc09969a',
            groups: [],
            controls: [],
            attributes: [],
            latest_version: ''
          }
        ],
        total: 1
      }));

    browser.get('/jobs/add');
  });

  // Note(sr) 2018-11-19: this is flakey, and has been disabled waiting for
  // someone to look into this.
  xdescribe('clicking cancel', () => {
    it('navigates back to `/jobs/add`', () => {
      const cancel = $('.job-add-actions chef-button[cancel]');

      expect(browser.getCurrentUrl()).toMatch(/\/jobs\/add$/);

      cancel.click();

      expect(browser.getCurrentUrl()).toMatch(/\/jobs$/);
    });
  });

  describe('creating a job', () => {

    describe('step 1: add nodes', () => {
      const managers = $$('.managers > chef-card');

      beforeEach(() => {
        browser.get('/jobs/add#add-nodes');
      });

      it('lists available nodemanagers', () => {
        expect(managers.count()).toEqual(6);
      });

      describe('toggling a nodemanager', () => {
        it('toggles the configuration form for that nodemanager', () => {
          const manager = $$('.managers > chef-card').first();
          const toggle = manager.$('.manager-toggle');
          const form = manager.$('.manager-body');

          expect(toggle.isSelected()).toEqual(false);
          expect(form.isPresent()).toEqual(false);

          toggle.click();

          expect(toggle.isSelected()).toEqual(true);
          expect(form.isPresent()).toEqual(true);
        });
      });

      it('requires selecting one or more nodemanagers', () => {
        const manager = $$('.managers > chef-card').first();
        const toggle = manager.$('.manager-toggle');
        const nextButton = element(by.cssContainingText('chef-button', 'Next'));
        const buttonEnabled = () => nextButton.getAttribute('disabled').then(val => val === null);

        expect(nextButton.getAttribute('disabled')).toEqual('true');

        toggle.click();

        browser.wait(buttonEnabled, 500, 'Next button should enable.');
        expect(nextButton.getAttribute('disabled')).toEqual(null);
      });

      describe('aws-api form', () => {
        const manager = managers.get(0);

        beforeEach(() => {
          manager.$('input[type="checkbox"]').click();
        });

        it('has fields for filtering by region', () => {
          const regionFields = manager.$('fieldset.filter-regions');
          expect(regionFields.isDisplayed()).toBe(true);
        });

        it('has a preview list of nodes', () => {
          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'aws-api-node-1',
            'aws-api-node-2',
            'aws-api-node-3'
          ]);
        });
      });

      describe('aws-ec2 form', () => {
        const manager = managers.get(1);

        beforeEach(() => {
          manager.$('input[type="checkbox"]').click();
        });

        it('has fields for filtering by region', () => {
          const regionFields = manager.$('fieldset.filter-regions');
          expect(regionFields.isDisplayed()).toBe(true);
        });

        it('has fields for filtering by tag', () => {
          const tagFields = manager.$('fieldset.filter-tags');
          expect(tagFields.isDisplayed()).toBe(true);
        });

        it('has a preview list of nodes', () => {
          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'aws-ec2-node-1',
            'aws-ec2-node-2',
            'aws-ec2-node-3'
          ]);
        });
      });

      describe('azure-api form', () => {
        it('has a preview list of nodes', () => {
          const manager = managers.get(2);
          manager.$('input[type="checkbox"]').click();

          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'azure-api-node-1',
            'azure-api-node-2',
            'azure-api-node-3'
          ]);
        });
      });

      describe('azure-vm form', () => {
        const manager = managers.get(3);

        beforeEach(() => {
          manager.$('input[type="checkbox"]').click();
        });

        it('has fields for filtering by region', () => {
          const regionFields = manager.$('fieldset.filter-regions');
          expect(regionFields.isDisplayed()).toBe(true);
        });

        it('has fields for filtering by tag', () => {
          const tagFields = manager.$('fieldset.filter-tags');
          expect(tagFields.isDisplayed()).toBe(true);
        });

        it('has a preview list of nodes', () => {
          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'azure-vm-node-1',
            'azure-vm-node-2',
            'azure-vm-node-3'
          ]);
        });
      });

      describe('gcp-api form', () => {
        it('has a preview list of nodes', () => {
          const manager = managers.get(4);
          manager.$('input[type="checkbox"]').click();

          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'gcp-api-node-1',
            'gcp-api-node-2',
            'gcp-api-node-3'
          ]);
        });
      });

      describe('automate form', () => {
        const manager = managers.get(5);

        beforeEach(() => {
          manager.$('input[type="checkbox"]').click();
        });

        it('has fields for filtering by name', () => {
          const nameFields = manager.$('fieldset.filter-names');
          expect(nameFields.isDisplayed()).toBe(true);
        });

        it('has fields for filtering by tag', () => {
          const tagFields = manager.$('fieldset.filter-tags');
          expect(tagFields.isDisplayed()).toBe(true);
        });

        it('has a preview list of nodes', () => {
          const list = manager.$('.manager-nodes-preview');
          const listItems = list.$$('chef-tr');

          browser.wait(EC.presenceOf(listItems.first()), 500, 'Loading indicator should go away');

          expect(list.isDisplayed()).toBe(true);
          expect(listItems.map(el => el.getText())).toEqual([
            'automate-node-1',
            'automate-node-2',
            'automate-node-3'
          ]);
        });
      });
    });

    describe('step 2: add profiles', () => {
      beforeEach(() => {
        browser.get('/jobs/add#add-profiles');
      });

      it('lists available profiles', () => {
        const profiles = $$('chef-table chef-tbody chef-tr');

        expect(profiles.count()).toEqual(1);
      });

      it('requires selecting one or more profiles', () => {
        const profile = $$('chef-table chef-tbody chef-tr').first();
        const toggle = profile.$('input[type="checkbox"]');
        const nextButton = element(by.cssContainingText('chef-button', 'Next'));
        const buttonEnabled = () => nextButton.getAttribute('disabled').then(val => val === null);

        expect(nextButton.getAttribute('disabled')).toEqual('true');

        toggle.click();

        browser.wait(buttonEnabled, 500, 'Next button should enable.');
        expect(nextButton.getAttribute('disabled')).toEqual(null);
      });
    });

    describe('step 3: add schedule', () => {
      beforeEach(() => {
        browser.get('/jobs/add#add-schedule');
      });

      describe('naming a job', () => {
        it('is required', () => {
          const form = $('.name-form');
          const input = form.$('input#name');
          const errors = form.$$('chef-error');

          expect(errors.count()).toEqual(0);

          input.sendKeys('');
          form.click();

          expect(errors.count()).toEqual(1);

          input.sendKeys('test-job-name');

          expect(errors.count()).toEqual(0);
        });
      });

      describe('scheduling a job', () => {
        it('is optional', () => {
          const scheduleToggle = $('.schedule-header input[type=checkbox]');

          expect(scheduleToggle.isSelected()).toEqual(false);
        });

        it('provides start datetime input', () => {
          const scheduleToggle = $('.schedule-header input[type=checkbox]');
          const startFormGroup = $('[formGroupName="start"]');
          const startInputs = startFormGroup.$$('.datetime select');

          scheduleToggle.click();

          expect(scheduleToggle.isSelected()).toEqual(true);
          expect(startInputs.count()).toEqual(6);
        });

        it('provides optional end datetime input', () => {
          const scheduleToggle = $('.schedule-header input[type=checkbox]');

          const endFormGroup = $('[formGroupName="end"]');
          const endFormGroupToggle = endFormGroup.$('[formControlName="include"]');
          const endInputs = endFormGroup.$$('.datetime select');

          scheduleToggle.click();

          expect(scheduleToggle.isSelected()).toEqual(true);
          expect(endFormGroupToggle.isSelected()).toEqual(false);
          expect(endInputs.count()).toEqual(0);

          endFormGroupToggle.click();

          expect(endFormGroupToggle.isSelected()).toEqual(true);
          expect(endInputs.count()).toEqual(6);
        });

        it('provides optional repeat interval input', () => {
          const scheduleToggle = $('.schedule-header input[type=checkbox]');

          const repeatFormGroup = $('[formGroupName="repeat"]');
          const repeatFormGroupToggle = repeatFormGroup.$('[formControlName="include"]');
          const repeatInput = repeatFormGroup.$('.field');

          scheduleToggle.click();

          expect(scheduleToggle.isSelected()).toEqual(true);
          expect(repeatFormGroupToggle.isSelected()).toEqual(false);
          expect(repeatInput.isPresent()).toEqual(false);

          repeatFormGroupToggle.click();

          expect(repeatFormGroupToggle.isSelected()).toEqual(true);
          expect(repeatInput.isPresent()).toEqual(true);
        });
      });
    });
  });
});
