var defaultConfig = require('./protractor.conf').config;

var debugConfig  = {

  capabilities: {
    browserName: 'chrome'
  }
};

exports.config = Object.assign(defaultConfig, debugConfig);
