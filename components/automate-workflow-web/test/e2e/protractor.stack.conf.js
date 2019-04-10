var defaultConfig = require('./protractor.conf').config;
var browserSupport = require('../browser_support.stack');
var StackTunnel = require('./tunnel.stack');

var now = Date.now();

var stackTunnel;
var multiCapabilities = browserSupport.map(function (browser) {
  return Object.assign(browser, {
    'build': 'e2e.build.' + now,
    'project': 'delivery',
    'name': 'e2e',
    'browserstack.user' : process.env.BROWSERSTACK_USER,
    'browserstack.key': process.env.BROWSERSTACK_KEY,
    'browserstack.local' : 'true',
    'browserstack.debug': 'true',
    'browserName': browser.browser
  });
});

var stackConfig = {

  multiCapabilities: multiCapabilities,

  beforeLaunch: function () {
    stackTunnel = new StackTunnel();
    return stackTunnel.start();
  },

  afterLaunch: function () {
    return stackTunnel.stop();
  },

  seleniumAddress: 'http://hub.browserstack.com/wd/hub'
};

exports.config = Object.assign(defaultConfig, stackConfig);
