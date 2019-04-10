// Protractor configuration file, see link for more information
// https://github.com/angular/protractor/blob/master/lib/config.ts

const { SpecReporter } = require('jasmine-spec-reporter');
const setupFakeServer = require('./e2e/helpers/fake_server').setupFakeServer;
const teardownFakeServer = require('./e2e/helpers/fake_server').teardownFakeServer;
const displayBrowserLogs = require('./e2e/helpers/browser_log').displayBrowserLogs;
const disableWelcomeModal = require('./e2e/helpers/welcome_modal').disableWelcomeModal;

exports.config = {
  allScriptsTimeout: 11000,
  specs: [
    './e2e/**/*.e2e-spec.ts'
  ],
  // https://github.com/angular/protractor/blob/15776b8aef301e3c0a3b1835df1a876414b2de14/docs/browser-setup.md#using-headless-chrome
  // Note: "As of Chrome 58 you also need to set --disable-gpu, though this may change in future versions."
  capabilities: {
    'browserName': 'chrome',
    chromeOptions: {
      args: [ "--headless", "--disable-gpu", "--no-sandbox" ]
    }
  },
  directConnect: true,
  baseUrl: 'http://localhost:4200/',
  framework: 'jasmine',
  jasmineNodeOpts: {
    showColors: true,
    defaultTimeoutInterval: 30000,
    print: function() {},
    random: true
  },
  onPrepare() {
    require('ts-node').register({
      project: 'e2e/tsconfig.e2e.json'
    });
    jasmine.getEnv().addReporter(new SpecReporter({ spec: { displayStacktrace: true } }));

    // Note 2019/02/06 (sr): Left this here on purpose in case someone needs it.
    // It can be hard to replicate the execution conditions on buildkite locally
    // and these values have served me well. 250ms seems to be just a little too
    // much.
    // browser.driver.setNetworkConditions({
    //   offline: false,
    //   latency: 200, // Additional latency (ms).
    //   download_throughput: 500 * 1024, // Maximal aggregated download throughput.
    //   upload_throughput: 500 * 1024 // Maximal aggregated upload throughput.
    // });

    beforeEach(setupFakeServer);
    afterEach(teardownFakeServer);

    afterEach(displayBrowserLogs);

    beforeAll(disableWelcomeModal);

    // Set the browser's resolution to our minimum supported value so that all
    // page elements are visible during tests.
    browser.driver.manage().window().setSize(1366, 768);
  }
};
