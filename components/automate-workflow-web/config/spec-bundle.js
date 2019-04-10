Error.stackTraceLimit = Infinity;

require('core-js/client/shim');
require('reflect-metadata');
require('zone.js/dist/zone');
require('zone.js/dist/long-stack-trace-zone');
require('zone.js/dist/proxy');
require('zone.js/dist/sync-test');
require('zone.js/dist/jasmine-patch');
require('zone.js/dist/async-test');
require('zone.js/dist/fake-async-test');
require('rxjs/Rx');

var testing = require('@angular/core/testing');
var browser = require('@angular/platform-browser-dynamic/testing');

beforeEach(() => {
  //spyOn(window.location, 'assign');
  // localStorage.setItem('canonical-enterprise', 'gotham');
});

testing.TestBed.initTestEnvironment(
  browser.BrowserDynamicTestingModule,
  browser.platformBrowserDynamicTesting()
);

function requireAll(requireContext) {
  return requireContext.keys().map(requireContext);
}

// Pull in test/unit/**/*.spec.js and src/**/*.spec.ts
var testContext =
  require.context('..', true, /test\/unit\/.+\.spec\.js$|src\/.+\.spec\.ts$/);

requireAll(testContext);
