import { browser, by, element } from 'protractor';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Event Feed: Guitar String Graph Interaction', () => {
  beforeAll(() => {
    browser.waitForAngularEnabled(false);
    browser.get('/event-feed');
  });

  it('should have a working dropdown filter', () => {
    element(by.css('app-event-feed-select chef-select')).click();

    const dropdown = element(by.css('chef-dropdown'));
    expect(dropdown.getAttribute('visible')).toEqual('true');
  });

  xit('Event Feed page is accessible', (done) => {
    expectUrlToBeAccessible('/', done);
  });
});
