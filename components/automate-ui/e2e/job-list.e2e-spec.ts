import { $, browser, ExpectedConditions as EC } from 'protractor';
import { fakeServer } from './helpers/fake_server';

describe('Job List', () => {
  const header = $('chef-page-header');
  const list = $('.jobs-list-table');

  beforeEach(() => {
    browser.waitForAngularEnabled(false);

    fakeServer()
      .post('/api/v0/compliance/scanner/jobs/search', JSON.stringify({
        filters: [
          {
            key: 'job_type',
            values: [
              'exec'
            ]
          },
          {
            key: 'parent_job',
            values: [
              ''
            ]
          }
        ],
        page: 1,
        per_page: 100,
        sort: 'end_time',
        order: 'DESC'
      }))
      .any()
      .reply(200, JSON.stringify({
        jobs: [
          {
            id: '983a37a1-6616-4887-97e6-b43cd73ba2e9',
            name: 'test-job-1',
            type: 'exec',
            timeout: 0,
            tags: [],
            start_time: '2018-10-18T08:44:04Z',
            end_time: '2018-10-18T08:44:06Z',
            status: 'completed',
            retries: 0,
            retries_left: 0,
            results: [],
            nodes: [],
            profiles: [],
            node_count: 1,
            profile_count: 2,
            node_selectors: [],
            scheduled_time: '0001-01-01T00:00:00Z',
            recurrence: '',
            parent_id: '',
            job_count: 0,
            deleted: false
          },
          {
            id: '5a00c4c9-6f38-47ff-b973-bbc42b598e30',
            name: 'test-job-2',
            type: 'exec',
            timeout: 0,
            tags: [],
            start_time: '0001-01-01T00:00:00Z',
            end_time: '0001-01-01T00:00:00Z',
            status: 'scheduled',
            retries: 0,
            retries_left: 0,
            results: [],
            nodes: [],
            profiles: [],
            node_count: 0,
            profile_count: 2,
            node_selectors: [],
            scheduled_time: '0001-01-01T00:00:00Z',
            recurrence: 'DTSTART=20181018T032000Z;UNTIL=20181019T035000Z;FREQ=MINUTELY;INTERVAL=1',
            parent_id: '',
            job_count: 0,
            deleted: false
          }
        ],
        total: 2
      }));

    browser.get('/compliance/scanner/jobs');
  });

  it('has a heading and a subheading', () => {
    const heading = header.$('chef-heading');
    const subheading = header.$('chef-subheading');

    expect(heading.getText())
      .toBe('Scan Jobs');
    expect(subheading.getText())
      .toBe('Compliance scan jobs run inspec exec on a set of nodes.');
  });

  it('has a list of jobs', () => {
    const nameColumn = list.$$('chef-tbody chef-tr:not(.new-row)').map(row => {
      return row.$('chef-td:first-child').getText();
    });

    expect(list.isDisplayed()).toEqual(true);
    expect(nameColumn).toEqual([
      'test-job-1',
      'test-job-2'
    ]);
  });

  describe('clicking delete job button', () => {
    const listItem = list.$$('chef-tbody chef-tr:not(.new-row)').first();
    const deleteBtn = listItem.$('chef-button[label="delete"]');
    const deletePrompt = $('#delete-prompt');

    beforeEach(() => {
      deleteBtn.click();
    });

    it('displays delete confirmation prompt', () => {
      expect(deletePrompt.isDisplayed()).toEqual(true);
    });
  });

  describe('delete confirmation prompt', () => {
    const listItem = list.$$('chef-tbody chef-tr:not(.new-row)').first();
    const deleteBtn = listItem.$('chef-button[label="delete"]');

    const deletePrompt = $('#delete-prompt');
    const confirmBtn = deletePrompt.$('chef-button[label="confirm"]');
    const cancelBtn = deletePrompt.$('chef-button[label="cancel"]');

    beforeEach(() => {
      deleteBtn.click();
    });

    it('asks if you are sure', () => {
      expect(deletePrompt.getText()).toContain('Are you sure you want to delete this job?');
    });

    it('provides buttons for confirming or cancelling the deletion', () => {
      expect(confirmBtn.isDisplayed()).toEqual(true);
      expect(cancelBtn.isDisplayed()).toEqual(true);
    });

    describe('clicking cancel button', () => {
      beforeEach(() => {
        cancelBtn.click();
        browser.wait(EC.invisibilityOf(deletePrompt), 5000);
      });

      it('hides delete confirmation prompt', () => {
        expect(deletePrompt.isDisplayed()).toEqual(false);
      });
    });

    describe('clicking confirm button', () => {

      beforeEach(() => {
        fakeServer()._assertions = [];

        fakeServer()
          .delete('/api/v0/compliance/scanner/jobs/id/983a37a1-6616-4887-97e6-b43cd73ba2e9')
          .reply(200);

        fakeServer()
          .post('/api/v0/compliance/scanner/jobs/search', JSON.stringify({
            filters: [
              {
                key: 'job_type',
                values: [
                  'exec'
                ]
              },
              {
                key: 'parent_job',
                values: [
                  ''
                ]
              }
            ],
            page: 1,
            per_page: 100,
            sort: 'end_time',
            order: 'DESC'
          }))
          .any()
          .reply(200, JSON.stringify({
            jobs: [
              {
                id: '5a00c4c9-6f38-47ff-b973-bbc42b598e30',
                name: 'test-job-2',
                type: 'exec',
                timeout: 0,
                tags: [],
                start_time: '0001-01-01T00:00:00Z',
                end_time: '0001-01-01T00:00:00Z',
                status: 'scheduled',
                retries: 0,
                retries_left: 0,
                results: [],
                nodes: [],
                profiles: [],
                node_count: 0,
                profile_count: 2,
                node_selectors: [],
                scheduled_time: '0001-01-01T00:00:00Z',
                recurrence:
                  'DTSTART=20181018T032000Z;UNTIL=20181019T035000Z;FREQ=MINUTELY;INTERVAL=1',
                parent_id: '',
                job_count: 0,
                deleted: false
              }
            ],
            total: 1
          }));

        confirmBtn.click();
      });

      it('hides delete confirmation prompt', () => {
        expect(deletePrompt.isDisplayed()).toEqual(false);
      });

      it('deletes the job', () => {
        const nameColumn = list.$$('chef-tbody chef-tr:not(.new-row)').map(row => {
          return row.$('chef-td:first-child').getText();
        });

        expect(list.isDisplayed()).toEqual(true);
        expect(nameColumn).toEqual(['test-job-1','test-job-2']);
      });
    });
  });
});
