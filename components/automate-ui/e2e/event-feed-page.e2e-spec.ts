import { browser, by, element } from 'protractor';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Event Feed: Guitar String Graph Interaction', () => {
  beforeAll(() => {
    browser.waitForAngularEnabled(false);
    browser.get('/event-feed');
  });

  it('should have a working dropdown filter', () => {
    const select = element(by.css('app-event-feed-select chef-select'));
    select.click();

    expect(select.getAttribute('class')).toContain('active');
  });

  xit('Event Feed page is accessible', (done) => {
    expectUrlToBeAccessible('/', done);
  });
});
