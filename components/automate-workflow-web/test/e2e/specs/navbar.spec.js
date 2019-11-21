import Navbar from '../page_objects/navbar.po';
import OrgsPage from '../page_objects/orgs.po';
import authorizedLogin from '../mocks/authorized_login.mock';

describe('navbar', () => {

  let navbar;

  beforeAll(login);

  afterAll(logout);

  afterEach(() => {
    browser.ignoreSynchronization = false;

    // Get back to Workflow app
    browser.get('/e/Chef/#/dashboard');
  });

  describe('navigation without a console link', () => {

    beforeEach(() => {
      navbar = new Navbar();

      mockApi([
        authorizedLogin,
        {
          request: {
            url: '/api/v0/e/Chef/users$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              users: []
            }
          }
        },
        {
          request: {
            url: '/api/v0/e/Chef/orgs$',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              orgs: []
            }
          }
        },
      ]);
    });

    // We have to ignore Angular synchronization because navigating to the
    // Visibility app currently means we're navigating outside the Workflow
    // app. See https://github.com/angular/protractor/issues/2643
    beforeEach(() => {
      browser.ignoreSynchronization = true;
      browser.get('#/dashboard');
      browser.wait(presenceOf('.navbar'));
    });

    describe('clicking the Automate logo', () => {

      it('navigates to the Automate welcome view', () => {
        navbar.logo.click();
        browser.wait(urlContains('/#/welcome'), 5000);
        expect(browser.driver.getCurrentUrl()).toContain('/#/welcome');
      });
    });

    describe('clicking "Workflow"', () => {

      it('navigates to the workflow dashboard view', () => {
        navbar.workflow.click();
        browser.wait(urlContains('/#/dashboard'), 5000);
        expect(browser.driver.getCurrentUrl()).toContain('/#/dashboard');
      });
    });

    describe('clicking "Admin"', () => {

      it('navigates to the admin view', () => {
        navbar.admin.click();
        browser.wait(urlContains('/#/users'), 5000);
        expect(browser.driver.getCurrentUrl()).toContain('/#/users');
      });
    });

    describe('clicking "Nodes"', () => {

      it('navigates to the visibility nodes view', () => {
        navbar.nodes.click();
        browser.wait(urlContains('/viz'), 5000);
        expect(browser.driver.getCurrentUrl()).toContain('/viz');
      });
    });

    describe('navigating to child state', () => {

      it('marks parent nav item as active', () => {
        let orgsPage = new OrgsPage();
        orgsPage.get();

        let itemContainer = navbar.workflow.element(by.xpath('..'));
        expect(itemContainer).toHaveClass('active');
      });
    });

    it('does not show any link next to Admin', () => {
      expect(navbar.consoleLink).not.toBePresent();
    });
  });
});
