import { browser, by, element } from 'protractor';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Event Feed: Guitar String Graph Interaction', () => {
  beforeAll(() => {
    browser.waitForAngularEnabled(false);
    browser.get('/event-feed');
  });

  it('should have a working dropdown filter', () => {
    const select = element(by.css('app-event-feed-select chef-select'));

    // Click the event feed dropdown filter and wait until the filter turns 'active'
    select.click().then(() => {
      return new Promise(function(resolve) {
        browser.wait(() => {
          return select.getAttribute('class').then((values: string) => {
            return values.indexOf('active') > 0;
          });
        // resolve the promise when the 'active' attribute is found or after one second
        }, 1000).then( () => resolve());
      });
    });
  });

  xit('Event Feed page is accessible', (done) => {
    expectUrlToBeAccessible('/', done);
  });
});
