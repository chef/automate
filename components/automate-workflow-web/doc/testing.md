# Delivery Web - Testing

Currently, there are two types of tests for the Delivery UI:

### Unit 

Tests which load individual components into the browser and test their behavior in isolation. Unit specs are defined within [Jasmine](http://jasmine.github.io/2.3/introduction.html) and executed by [Karma](http://karma-runner.github.io).

### E2E 

Tests which load the full application into the browser within a mocked server environment and test the application behavior from the perspective of a user. E2E specs are defined within [Jasmine](http://jasmine.github.io/2.3/introduction.html) and executed by [Protractor](http://www.protractortest.org).

## Running

Ensure you have needed dependencies:

    make install

To run both unit and e2e tests:

    make test

### Unit

When unit tests are run, Karma loads up source files in the `src` directory and spec files in the `test/unit/specs` directory into the browser, executes the specs against the application source code, and then reports the results.

To run unit tests:

    make unit

To run unit tests continuously during development:

    make unit-debug

To run specific spec files:

    # Runs /specs/version.spec.js
    make unit specs=version

    # Runs /specs/version.spec.js, /specs/routes/change/change_controller.spec.js
    make unit specs=version,routes/change/change_controller

### E2E

When e2e tests are run, Protractor will spin up a mock server for the application to run against and a [Selenium](https://angular.github.io/protractor/#/server-setup) server for communicating with the browser(s) running the application. Once these prerequisites are running, the application will be opened in the browser, the specs in the `test/e2e/specs` directory will be executed against the running application, the browser and the aforementioned prerequisites will be shutdown, and the results will be reported.

To run e2e tests:

    make e2e

To run e2e tests in debug mode during development:

    make e2e-debug

To run specific specs, please change the `describe` blocks you're interested in to `fdescribe` and run `make e2e`.

#### Browserstack

To run tests in Browserstack, you'll need to store the username and access key for your account in environment variables before running the tests.

To run both unit and e2e tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make test-stack

To run unit tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make unit-stack

To run e2e tests:

    BROWSERSTACK_USER=johndoe BROWSERSTACK_KEY=8f8ajj22af88 make e2e-stack

##### Mock server

Protractor loads the e2e specs and executes them against the full running application in the browser. For the application to run this requires a server to respond to all requests for static assets (styles, images, scripts, etc.) and all API requests made by the application during its runtime.

To do this, an [Express](http://expressjs.com) server is used to serve all the static assets and to also proxy any API requests to a [Stubby](http://stub.by) server. During the specs' execution, mock endpoint data is loaded into and cleared from the API server for each spec. More detail on the process of loading and clearing mock endpoint data can be found in the *Writing* section.

## Writing

### Unit

Unit specs are written within Jasmine and get executed in the browser by Karma.

TODO: Talk about writing unit tests for the various types of components (services, directives, controllers, etc.) that the application consists of.

##### Resources

- [https://quickleft.com/blog/angularjs-unit-testing-for-real-though/](https://quickleft.com/blog/angularjs-unit-testing-for-real-though/)
- [www.yearofmoo.com/2013/01/full-spectrum-testing-with-angularjs-and-karma.html](www.yearofmoo.com/2013/01/full-spectrum-testing-with-angularjs-and-karma.html)
- [http://www.smashingmagazine.com/2014/10/07/introduction-to-unit-testing-in-angularjs/](http://www.smashingmagazine.com/2014/10/07/introduction-to-unit-testing-in-angularjs/)

### E2E

E2E specs are written within Jasmine and get executed in the Node environment by Protractor. 

##### Interacting with the browser

TODO: Talk about `protractor`, `browser`, `element` and various other tools you'll use in your e2e specs for exercising the application in the browser and making assertions on its behavior.

##### Mocking API requests

API requests are mocked using a Stubby server that has its configured endpoint responses available to be loaded and cleared by each spec during their execution. It looks like this:

    // For this spec, respond with a 401 to POST /get-token
    mockApi([
      {
        request: {
          url: '/api/v0/e/Chef/get-token',
          method: 'POST'
        },
        response: {
          status: 401
        }
      }
    ]);

    // Do your spec

    // Clean up for the next spec by clearing out mocked API endpoints
    mockApi.clear();

`mockApi()` is a global helper function available to the specs that takes an array of endpoint config objects which will be used to match any incoming API requests coming from the application with the desired API response for a particular spec.

Commonly used endpoint config objects can be stored in a separate file and imported into the spec:

    import unauthorizedLogin from '../mocks/unauthorized_login.mock';

    mockApi([
      unauthorizedLogin
    ]);

For more information on the format of these endpoint config objects see the Stubby docs on [Endpoint Configuration](https://github.com/mrak/stubby4node#endpoint-configuration).

Calling `mockApi(endpoints)` multiple times will push the endpoint config objects into collection of endpoint objects that are already on the API server which could be problematic when mocking out different responses for identical requests between specs. As such, you'll often want to be sure to clear out the API server after the end of a spec using `mockApi.clear()`. Fortunately, we currently have Jasmine set up to automatically clear the API server after each spec so you shouldn't need to actually use `mockApi.clear()` that often.

##### Mocking SSE endpoints

In addition to data being fetched from API requests, parts of the application stream in data via SSE (server-sent events) endpoints. These SSE endpoints need to be mocked out as well during E2E specs. It looks like  this:

    // For this spec, we need to mock the SSE endpoint for /changes/9fa0ff/streaming
    let mockStream = mockSse('/api/v0/e/Chef/changes/9fa0ff/streaming');

    // Do your spec

    // Send an event to the mockstream
    mockStream.send('eventName', 'some event data');

    // Do some more spec

    // Clean up for the next spec by clearing out mocked SSE endpoints
    mockSse.clear();

`mockSse(endpoint)` is a global helper function available to the specs that takes a URL string and returns a `mockStream` instance. This `mockStream` object has a `send(name, data, id)` method that allows you to send SSE events to the stream.

Calling `mockSse(endpoint)` multiple times for the same endpoint would override the previously mocked endpoint which could be problematic when mocking out the same SSE endpoint between specs. As such, you'll want to be sure to remove any mocked SSE endpoints after the end of a spec using `mockSse.clear()`. Fortunately, we currently have Jasmine set up to automatically clear away any mocked SSE endpoints after each spec so you shouldn't need to actually use `mockSse.clear()` that often.

##### Authentication

To reach the vast majority of the application the user is required to log in. As such, you'll need to log into the app before many of your specs can execute successfully. To make this task as easy as possible there's a global `login()` helper available; simply call it before your specs:

    // Log into the app
    beforeAll(login);

    // Do your specs

Specs that execute later may rely on the application starting from an un-authenticated state so you'll want to log back out after your specs run. To do this there is a `logout()` helper available; simply call it after your specs:

    // Do your specs

    // Log out of the app
    afterAll(logout);

You can execute these helpers in `beforeEach/afterEach` blocks but logging in and out for each spec in your spec file will slow them down quite a bit.

##### Page Objects

TODO: Talk about Page Object pattern and how to write them.

##### Full example
    
    import LoginPage from '../page_objects/login.po';

    describe('logging in', () => {

      let loginPage;

      beforeEach(() => {
        loginPage = new LoginPage();
      });

      describe('with invalid credentials', () => {

        beforeEach(() => {
          // Mock a 401 response /get-token
          mockApi([
            {
              request: {
                url: '/api/v0/e/Chef/get-token',
                method: 'POST'
              },
              response: {
                status: 401
              }
            }
          ]);
          
          // Navigate to the login page
          loginPage.get();

          // Type in wrong credentials and click submit
          loginPage.login('grrm', 'wrongpassword');
        });

        it('should not log you in', () => {
          // Assert that we're still on the login page
          expect(browser.getCurrentUrl()).toMatch(/.*login$/);
        });

        it('should display an error message', () => {
          // Assert that we see an error message
          expect(loginPage.messageText).toBeDisplayed();
          expect(loginPage.messageText).toHaveClass('error');
        });
      });

      describe('with valid credentials', () => {

        beforeEach(() => {
          // Mock a 201 response /get-token
          // Mock a 200 response /orgs
          mockApi([
            {
              request: {
                url: '/api/v0/e/Chef/get-token',
                method: 'POST'
              },
              response: {
                status: 201,
                body: {
                  token: 'Qm+yP7LX1l/I1xW5pJg8NfEhgE1COkTeb3auVkRmQfc='
                }
              }
            },
            {
              request: {
                url: '/api/v0/e/Chef/orgs',
                method: 'GET'
              },
              response: {
                status: 200,
                body: {
                  orgs: [
                    {
                      name: 'FooOrg',
                      project_count: 2
                    },
                    {
                      name: 'BarOrg',
                      project_count: 1
                    }
                  ]
                }
              }
            }
          ]);

          // Navigate to the login page
          loginPage.get();

          // Type in correct credentials and click submit
          loginPage.login('grrm', 'grrm');
        });

        it('should log you in', () => {
          // Assert that we've been redirected to the organizations page
          expect(browser.getCurrentUrl()).toMatch(/.*organizations$/);
        });
      });
    });

##### Resources

- [http://www.ng-newsletter.com/posts/practical-protractor.html](http://www.ng-newsletter.com/posts/practical-protractor.html)
- [http://www.thoughtworks.com/insights/blog/using-page-objects-overcome-protractors-shortcomings](http://www.thoughtworks.com/insights/blog/using-page-objects-overcome-protractors-shortcomings)

## Debugging

### Unit

To debug unit tests:

1. Run `make unit-debug`.
2. Open the developer tools (`Cmd+Opt+I`) in the launched Chrome instance.
3. Place breakpoints (`debugger;`) at the locations in your source or spec code where you want to pause the test runner and inspect the current state.
4. Save these changes and the tests will re-run, halting at each breakpoint you placed.
5. Debug/inspect your tests.
6. `Ctrl+C` to quit.

### E2E

To debug e2e tests:

1. Place `browser.pause()` at the locations in your spec code where you want to pause the test runner and inspect the current state.
2. Run `make e2e-debug`.
3. During the execution of the specs Protractor will pause the browser at the locations you specified.
4. Debug/inspect the application via the browser or the interactive mode now available in the terminal.
5. In the terminal, type `repl` to enter interactive mode where you can send [WebDriver commands](https://angular.github.io/protractor/#/api) to the browser.
6. Type `exit` to leave interactive mode.
7. `Ctrl+C` twice to quit.

See [Protractor's docs on debugging](https://angular.github.io/protractor/#/debugging) to get more detailed information on debugging your e2e tests and more information on additional debugging features that are available.
