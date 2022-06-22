// Karma configuration file, see link for more information
// https://karma-runner.github.io/1.0/config/configuration-file.html

module.exports = function (config) {
  config.set({
    basePath: '',
    frameworks: ['jasmine', '@angular-devkit/build-angular'],
    plugins: [
      require('karma-jasmine'),
      require('karma-chrome-launcher'),
      require('karma-jasmine-html-reporter'),
      require('karma-firefox-launcher'),
      require('karma-safari-launcher'),
      require('karma-coverage'),
      require('@angular-devkit/build-angular/plugins/karma')
    ],
    files: [
      {pattern: './node_modules/@angular/material/prebuilt-themes/deeppurple-amber.css', included: true, watched: true},
      {pattern: 'jasmine.conf.js', included: true, watched: true},

      // Includes all package tests and source files into karma. Those files will be watched.
      // This pattern also matches all all source map files and TypeScript files for debugging.
      // {pattern: 'node_modules/**/*', included: false, watched: true}
    ],
    client:{
      clearContext: false // leave Jasmine Spec Runner output visible in browser
    },

    coverageReporter: {
      dir: require('path').join(__dirname, 'coverage'),
      subdir: '.',
      reporters: [
        { type: 'html' },
        { type: 'lcovonly' },
        { type: 'json-summary' }
      ],
    },
    
    reporters: ['dots', 'kjhtml'],
    port: 9876,
    colors: true,
    logLevel: config.LOG_INFO,
    autoWatch: true,
    // 1. Installed browser choices: Chrome, Firefox, Safari, ChromeHeadless
    // 2. Be sure *not* to commit with 'Safari' because this needs to work
    //    on linux too, not just mac.
    //    (Should really only commit with a headless choice enabled anyway.)
    // 3. You do not necessarily need to modify this file to run different browsers;
    //    just run e.g. "ng test --browsers=Firefox" to select from the command line.
    browsers: process.env.CI ? ['ChromeHeadlessNoSandbox'] : ['ChromeHeadless'],
    customLaunchers: {
      ChromeHeadlessNoSandbox: {
        base: 'ChromeHeadless',
        flags: ['--no-sandbox']
      }
    },
    singleRun: false,
    // https://docs.travis-ci.com/user/gui-and-headless-browsers/#Karma-and-Firefox-inactivity-timeouts
    browserNoActivityTimeout: 30000,
  });
};
