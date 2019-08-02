import { browser } from 'protractor';
import { expectUrlToBeAccessible } from './helpers/accessibility_helpers';

describe('Event Feed: Guitar String Graph Interaction', () => {
  beforeAll(() => {
    browser.waitForAngularEnabled(false);
    browser.get('/event-feed');
  });

  xit('Event Feed page is accessible', (done) => {
    expectUrlToBeAccessible('/', done);
  });
});
