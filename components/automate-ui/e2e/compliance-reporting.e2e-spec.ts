import { $, $$, browser } from 'protractor';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Compliance Reporting', () => {

  beforeEach(() => {
    browser.waitForAngularEnabled(false);
  });

  it('has a heading and a subheading', () => {
    browser.get('/compliance/reporting/overview');

    const heading = $('chef-page-header chef-heading');
    const subheading = $('chef-page-header chef-subheading');

    expect(heading.getText()).toBe('Reports');
    expect(subheading.getText()).toBe([
      'Compliance reports describe the status of scanned infrastructure. Filtering by a profile,',
      'or a profile and one associated control, will enable deep filtering, which will also',
      'reflect on the status of the node.'
    ].join(' '));
  });

  describe('filtering', () => {
    const filterButtons = $$('.filters-list chef-button:not(:first-child)');

    describe('by default', () => {
      it('has no filters', () => {
        browser.get('/compliance/reporting');

        expect(filterButtons.count()).toEqual(0);
      });
    });

    describe('by job ID', () => {
      const id = '98675307-7523-465d-b98d-81022e7a64b0';

      it('sets job_id filter', () => {
        browser.get(`/compliance/reporting/overview?job_id=${id}`);

        expect(filterButtons.count()).toEqual(1);
        expect(filterButtons.first().getText()).toContain(`job_id: ${id}`);
      });

      describe('clicking job_id filter', () => {
        it('removes job_id filter', () => {
          browser.get(`/compliance/reporting/overview?job_id=${id}`);

          expect(filterButtons.count()).toEqual(1);
          filterButtons.first().click();

          expect(filterButtons.count()).toEqual(0);
          expect(browser.getCurrentUrl()).not.toContain(`job_id=${id}`);
        });
      });
    });
  });

  xit('is accessible', done => {
    expectUrlToBeAccessible('/compliance/reporting', done);
  });
});
