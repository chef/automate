import Dashboard from '../page_objects/dashboard.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('dashboard page', () => {

  let dashboard, mocks;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    dashboard = new Dashboard();

    mockApi.clear();

    mocks = [
      {
        request: {
          url: '/api/v0/e/Chef/pipeline_status',
          method: 'GET'
        },
        response: {
          body: [{
            "id":"fb854bd7-866d-4ef6-b04f-bc4b5eecde7d",
            "title":"Pull in latest vendored cookbooks",
            "org":"Chef_Delivery",
            "project":"delivery",
            "stage":"verify",
            "stage_status":"running",
            "submitter":"duffieldt",
            "submitted_at":"2015-09-17 16:39:35",
            "approved_by":null,
            "delivered_by":null,
            "includes": []
          }]
        }
      },
      authorizedLogin
    ];
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

  describe('when you click on a project row', () => {

    beforeEach(() => {
      mockApi(mocks);
      dashboard.get();
      dashboard.projects.first().click();
    });

    it('opens the project', () => {
      expect(dashboard.projectChanges.count()).toBe(1);
    });

    describe('and then navigate away and return', () => {

      beforeEach(() => {
        browser.get('#/organizations');
        dashboard.get();
      });

      xit('remembers your selection', () => {
        expect(dashboard.projectChanges.count()).toBe(1);
      });
    });
  });

  describe('when a change is superseded', () => {

    beforeEach(() => {

      mocks = [
        {
          request: {
            url: '/api/v0/e/Chef/pipeline_status',
            method: 'GET'
          },
          response: {
            body: [{
              "id": "fb854bd7-866d-4ef6-b04f-bc4b5eecde7d",
              "title": "Pull in latest vendored cookbooks",
              "org": "Chef_Delivery",
              "project": "delivery",
              "stage": "union",
              "stage_status": "running",
              "submitter": "duffieldt",
              "submitted_at": "2015-01-05 00:14:51",
              "approved_by": "duffieldt",
              "delivered_by": "duffieldt",
              "includes": [
                {
                  "id": "e5de2039-e3cc-42b4-b317-3a8853a2713f",
                  "title": "Reorder software definitions",
                  "org": "Chef_Delivery",
                  "project": "delivery",
                  "submitted_at": "2015-01-04 00:13:24",
                  "submitter": "cnunciato",
                  "stage": "acceptance",
                  "stage_status": "failed"
                }
              ]
            }]
          }
        },
        authorizedLogin
      ]
    });

    describe('in the shared pipeline', () => {

      beforeEach(() => {
        mockApi(mocks);
        dashboard.get();
      });

      it('renders the superseded change', () => {
        expect(dashboard.includedChanges.count()).toBe(1);
      });
    });

    describe('in a change contained in a project', () => {

      beforeEach(() => {
        let change = mocks[0].response.body[0];

        change.stage = 'acceptance';
        change.stage_status = 'running';
        change.delivered_by = null;

        mockApi(mocks);
        dashboard.get();

        dashboard.projects.first().click();
      });

      it('renders the superseded change', () => {
        expect(dashboard.includedChanges.count()).toBe(1);
      });
    });
  });

  describe('filtering', () => {

    beforeEach(() => {

      mocks[0].response.body.push({
        "id": "8a705438-69f9-44f9-94ea-c2b9a2a567fa",
        "title": "Add something awesome",
        "org": "Chef_Delivery",
        "project":"delivery-cli",
        "stage": "verify",
        "stage_status": "passed",
        "submitter": "mcampbell",
        "submitted_at": "2015-11-03 17:49:32",
        "approved_by": null,
        "delivered_by": null,
        "includes": []
      });

      mockApi(mocks);
      dashboard.get();
    });

    describe('by typing a filter expression', () => {

      beforeEach(() => {
        dashboard.filterInput.sendKeys('project:delivery-cli');
      });

      it('filters the result set', () => {
        expect(dashboard.projects.count()).toEqual(1);
      });

      // Marked pending because of occasional failures
      xdescribe('and then pressing escape', () => {

        beforeEach(() => {
          dashboard.filterInput.sendKeys(protractor.Key.ESCAPE);
        });

        it ('clears the filter', () => {
          expect(dashboard.projects.count()).toEqual(2);
        });
      });

      describe('and then navigating away and returning', () => {

        beforeEach(() => {
          browser.get('#/audit');
          dashboard.get();
        });

        // Marked pending because of occasional failures
        xit('remembers the filter', () => {
          expect(dashboard.projects.count()).toEqual(1);
        });
      });
    });

    describe('by clicking an object', () => {

      beforeEach(() => {
        dashboard.approvables.click();
      });

      it('filters the result set', () => {
        expect(dashboard.projects.count()).toEqual(1);
      });

      it('shows the filter expression', () => {
        expect(dashboard.filterInput.getAttribute('value')).toEqual('stage:verify status:passed');
      });

      describe('and then clicking the clear button', () => {

        beforeEach(() => {
          dashboard.clearButton.click();
        });

        it ('clears the filter', () => {
          expect(dashboard.projects.count()).toEqual(2);
        });
      });
    });
  });

  describe('icons', () => {

    beforeEach(() => {

      mocks[0].response.body.push({
        "id": "8a705438-69f9-44f9-94ea-c2b9a2a567fa",
        "title": "Add something awesome",
        "org": "Chef_Delivery",
        "project":"delivery-cli",
        "stage": "verify",
        "stage_status": "passed",
        "submitter": "mcampbell",
        "submitted_at": "2015-11-03 17:49:32",
        "approved_by": null,
        "delivered_by": null,
        "includes": []
      });

      mockApi(mocks);
      dashboard.get();
    });

    describe('a project that has a change in verify with the status of passed', () => {
      beforeEach(() => {
        dashboard.filterInput.sendKeys('project:delivery-cli');
      });

      describe('when collapsed', () => {
        it('has a ribbon icon of a human', () => {
          let icon = dashboard.ribbonHumanIcon;
          expect(icon).toBePresent();
        });

        it('has an icon of a human in the project row', () => {
          let icon = dashboard.projectHumanIcon;
          expect(icon).toBePresent();
        });
      })

      describe('when expanded', () => {
        beforeEach(() => {
          dashboard.firstProject.click();
        });

        it('has an icon of a human in the change row when you roll it down', () => {
          let icon = dashboard.changeHumanIcon;
          expect(icon).toBePresent();
        });
      });
    });
  });
});
