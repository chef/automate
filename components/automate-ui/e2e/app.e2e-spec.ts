import { browser, by, element, $, ExpectedConditions as EC } from 'protractor';
import { fakeServer } from './helpers/fake_server';
import { enableWelcomeModal, disableWelcomeModal } from './helpers/welcome_modal';

describe('App', () => {
  it('lands on the event feed page', () => {
    browser.waitForAngularEnabled(false);
    browser.get('/');

    expect(browser.getCurrentUrl()).toMatch(/\/event-feed$/);
  });
});

// Disabling these tests until we can figure out how to
// dispatch a TriggerWelcome action that would make the welcome modal appear.
xdescribe('Welcome Modal', () => {
  it('shows the expected Header when visible', () => {
    enableWelcomeModal();

    browser.waitForAngularEnabled(false);
    browser.get('/');

    const welcome_modal_header = $('.welcome-modal .display2');

    expect(welcome_modal_header.getText()).toBe('Welcome to Chef Automate');
  });

  it('opens a new window for docs and learn chef', () => {
    enableWelcomeModal();

    browser.waitForAngularEnabled(false);
    browser.get('/');

    // This test is temporarily disabled until automate.chef.io/docs is live
    // element(by.css('.help-link')).click().then(function () {
    //   browser.getAllWindowHandles().then(function (handles) {
    //     browser.switchTo().window(handles[handles.length - 1]).then(function () {
    //         expect(browser.getCurrentUrl()).toEqual('https://automate.chef.io/docs');
    //     });

    //     // switch back to the main window
    //     browser.switchTo().window(handles[0]);
    //   });
    // });

    element(by.css('.teach-link')).click().then(function () {
      browser.getAllWindowHandles().then(function (handles) {
        browser.switchTo().window(handles[handles.length - 1]).then(function () {
          expect(browser.getCurrentUrl()).toEqual('https://learn.chef.io/#/');
        });

        // switch back to the main window
        browser.switchTo().window(handles[0]);
      });
    });
  });

  it('should not have show on launch option checked', () => {
    enableWelcomeModal();

    browser.waitForAngularEnabled(false);
    browser.get('/');

    expect($('.show-again-checkbox').isSelected()).toBe(false);
  });

  it('is not present when close link is clicked', () => {
    enableWelcomeModal();
    browser.waitForAngularEnabled(false);
    browser.get('/');
    $('.close-button').click();
    const app_welcome_modal = element(by.tagName('app-welcome-modal'));
    expect(app_welcome_modal.isElementPresent(by.css('.welcome-modal'))).toBe(false);
  });
});

describe('Main Navigation', () => {
  beforeAll(disableWelcomeModal);

  xit('redirects to Client Runs', () => {
    browser.waitForAngularEnabled(false);
    element(by.linkText('Client Runs')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/client-runs/);
  });

  it('redirects to Event Feed', () => {
    browser.waitForAngularEnabled(false);
    element(by.linkText('Event Feed')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/event-feed/);
  });

  // TODO: this now depends on authz introspection -- it's hidden in the e2e
  // situation, it seems
  xit('redirects to Admin', () => {
    browser.waitForAngularEnabled(false);
    element(by.linkText('Admin')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/admin\/users/);
  });

  // All of the below pending tests are targeting the correct link but further work
  // is needed to deal with the timeouts that are happening on Compliance Page loads
  xit('redirects to Compliance', () => {
    element(by.linkText('Compliance')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/compliance\/reporting\/overview/);
  });

  xit('redirects to Scan Jobs', () => {
    element(by.linkText('Scan Jobs')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/compliance\/scanner\/jobs/);
  });

  xit('redirects to Asset Store', () => {
    element(by.linkText('Asset Store')).click();
    expect(browser.getCurrentUrl()).toMatch(/\/profiles/);

  });

  xit('redirects to Credentials', () => {
    element(by.css('#nav-icon-link-2')).click();
    browser.wait(EC.urlContains('node-credentials'));
    expect(browser.getCurrentUrl()).toMatch(/\/settings\/node-credentials/);
  });

  xit('redirects to Node Integrations', () => {
    element(by.css('#nav-icon-link-3')).click();
    browser.wait(EC.urlContains('node-integrations'));
    expect(browser.getCurrentUrl()).toMatch(/\/settings\/node-integrations/);
  });
});

describe('User Dropdown', () => {

  it('displays the version text', () => {
    const expectedVersion = '20180416135645';
    const body = `{"build_timestamp":"${expectedVersion}"}`;

    fakeServer().get('/api/v0/version').many().reply(200, body);

    browser.waitForAngularEnabled(false);
    browser.get('/');

    element(by.css('app-profile')).click();

    const versionLink = $('.version');
    expect(versionLink.getText()).toBe(`Version: ${expectedVersion}`);
  });

  it('shows the welcome modal when "About Chef Automate" clicked', () => {
    enableWelcomeModal();

    browser.waitForAngularEnabled(false);
    browser.get('/');

    browser.wait(EC.visibilityOf(element(by.css('app-profile'))));

    element(by.css('app-profile')).click();

    const aboutChefAutomate = $('app-profile button.welcome-modal-button');
    aboutChefAutomate.click();

    const welcomeModalHeader = $('.welcome-modal .display2');
    browser.wait(EC.visibilityOf(welcomeModalHeader));
    expect(welcomeModalHeader.getText()).toBe('Welcome to Chef Automate');
  });
});
