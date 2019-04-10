require('babel-register');
require('./helpers/helpers');

var MockServer = require('./mock_server').default;
var mockServer;

var specs = [
  './specs/**/*.spec.js'
];

if (process.env.SPECS) {
  specs = process.env.SPECS.split(',').map(function (spec) {
    return './specs/' + spec + '.spec.js';
  });
}

var defaultConfig = {

  framework: 'jasmine2',

  specs: specs,

  baseUrl: 'http://localhost:9000/e/Chef',

  rootElement: 'html',

  onPrepare: function () {

    beforeEach(function () {
      jasmine.addMatchers(require('./matchers').default);

      var width = 1024;
      var height = 768;
      browser.driver.manage().window().setSize(width, height);

      // Tell Protractor we're running both Angular 1 and 2 concurrently
      browser.ng12Hybrid = true;
    });

    // Disable animations to speed up tests
    var disableNgAnimate = function () {
      angular
        .module('disableNgAnimate', [])
        .run(['$animate', function ($animate) {
          $animate.enabled(false);
        }]);
    };

    browser.addMockModule('disableNgAnimate', disableNgAnimate);

    afterEach(() => {
      mockApi.clear();
      mockSse.clear();
      browser.ng12Hybrid = false;
    });

    mockServer = new MockServer();
    return mockServer.start();
  },

  onComplete: function () {
    return mockServer.stop();
  },

  maxSessions: 1,

  allScriptsTimeout: 180000,

  getPageTimeout: 180000,

  seleniumAddress: process.env.SELENIUM_HEAD || null
};

exports.config = defaultConfig;
