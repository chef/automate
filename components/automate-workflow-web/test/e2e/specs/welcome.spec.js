describe('the workflow welcome view', () => {

  describe('when a canonical_enterprise does not exist', () => {

    beforeEach(() => {
      mockApi([
        {
          request: {
            url: '/api/v0/canonical_enterprise',
            method: 'GET'
          },
          response: {
            status: 404
          }
        }
      ]);

      browser.get('#/welcome');
    });

    it('provides useful messaging', () => {
      let content = element(by.css('.welcome-view')).getText();
      expect(content).toContain('You need to create an enterprise.');
    });

    it('links to the automate-ctl documentation', () => {
      let docsLink = element(by.css('.welcome-view p a'));
      expect(docsLink.getAttribute('href'))
        .toBe('https://docs.chef.io/ctl_delivery_server.html#create-enterprise');
    });

    describe('and one is created', () => {

      beforeEach(() => {
        mockApi.clear();

        mockApi([
          {
            request: {
              url: '/api/v0/canonical_enterprise',
              method: 'GET'
            },
            response: {
              status: 200,
              body: {
                ent_name: 'Chef'
              }
            }
          }
        ]);
      });

      describe('the reload button', () => {

        // We're redirecting out of the Angular 1 app here. This tells
        // Protractor to ignore the absence of Angular.

        beforeEach(() => {
          browser.ignoreSynchronization = true;
        });

        afterEach(() => {
          browser.ignoreSynchronization = false;
        });

        it('redirects to the login view', () => {
          expect(browser.getCurrentUrl()).toMatch(/\/e\/Chef\/#\/welcome$/);

          element(by.buttonText("I'm Ready to Sign In")).click();
          expect(browser.getCurrentUrl()).toMatch(/\/e\/Chef\/#\/login/);
        });
      });
    });
  });

  describe('when a canonical_enterprise exists', () => {

    beforeEach(() => {
      mockApi([
        {
          request: {
            url: '/api/v0/canonical_enterprise',
            method: 'GET'
          },
          response: {
            status: 200,
            body: {
              ent_name: 'Chef'
            }
          }
        }
      ]);

      browser.get('#/welcome');
    });

    beforeEach(() => {
      browser.ignoreSynchronization = true;
    });

    afterEach(() => {
      browser.ignoreSynchronization = false;
    });

    it('redirects to the login view', () => {
      expect(browser.getCurrentUrl()).toMatch(/\/e\/Chef\/#\/login/);
    });
  });
});
