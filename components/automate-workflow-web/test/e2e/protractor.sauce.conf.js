var defaultConfig = require('./protractor.conf').config;
var browserSupport = require('../browser_support.sauce');
var SauceTunnel = require('./tunnel.sauce');

var sauceTunnel;

var stackConfig = {

  multiCapabilities: browserSupport,

  beforeLaunch: function () {
    sauceTunnel = new SauceTunnel();
    return sauceTunnel.start();
  },

  afterLaunch: function () {
    return sauceTunnel.stop();
  },

  sauceUser: process.env.SAUCE_USER,

  sauceKey: process.env.SAUCE_KEY,

  sauceSeleniumAddress: 'localhost:4445/wd/hub'
};

exports.config = Object.assign(defaultConfig, stackConfig);
