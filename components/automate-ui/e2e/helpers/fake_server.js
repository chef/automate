// Documentation for hock can be found on its github page:
// https://github.com/mmalecki/hock
const createHock = require('hock').createHock;
const createServer = require('http').createServer;

// This needs to be the same as the port used in the e2e test proxy config file,
// proxy.conf.e2e.json
const fakeServerPort = 4201;
let hock, server;

// A function meant to be called in a before hook.
var setupFakeServer = function(done) {
  // The throwOnUnmatched flag controls whether the HTTP handler raises an
  // error when an HTTP request is made to a URL that was not stubbed. Setting
  // it to false means the handler will print a warning when a request is made
  // to unrecognized URLs.
  hock = createHock({throwOnUnmatched: false});

  // Create an HTTP server to respond to our stubbed requests and wait for it
  // to be available before running the test code.
  server = createServer(hock.handler);
  server.listen(fakeServerPort, done);
};

// A function meant to be called in an after hook.
var teardownFakeServer = function(done) {
  // Calling hock.done() after the test has run tells hock to verify that all
  // stubbed requests were made and that all constraints placed on the stubbed
  // requests were satisfied.
  hock.done();

  // Tell the HTTP server to shutdown and wait for it to succeed before running
  // more tests.
  server.close(done);
};

var fakeServer = function() {
  return hock;
}

module.exports = {};
module.exports.setupFakeServer = setupFakeServer;
module.exports.teardownFakeServer = teardownFakeServer;
module.exports.fakeServer = fakeServer;
