import * as AxeBuilder from 'axe-webdriverjs';
import { browser } from 'protractor';

const buildErrorOutput = function(violations) {
  if(violations.length === 0) { return '' };

  const output = violations.map((violation) => {
    const selectors = violation.nodes.map((node) => {
      return ' - ' + node.target.join(' ')
    }).join('\n')

    return violation.help + '\n' + selectors
  }).join('\n');

  return '\n' + output + '\n';
};

// Meant to be called from a test after protractor's browser has been put in
// the desired state.
// TODO: Currently disables tests for color contrast.
//
// The format for the options object parameter can be found here:
// https://github.com/dequelabs/axe-core/blob/009fb3535d928a9ea9d5febf4cc5eb969a06af07/doc/API.md#options-parameter
//
// The format of the results object returned can be found here:
// https://github.com/dequelabs/axe-core/blob/009fb3535d928a9ea9d5febf4cc5eb969a06af07/doc/API.md#results-object
const expectPageToBeAccessible = function(done) {
  AxeBuilder(browser)
    .options({"rules": { "color-contrast": { enabled: false } } })
    .analyze(function(results) {
      const errorOutput = buildErrorOutput(results.violations);
      expect(errorOutput).toBe('');
      done();
    });
};

// Navigates protractor's browser to the desired URL then runs an a11y test.
const expectUrlToBeAccessible = function(url, done) {
  browser.get(url);
  expectPageToBeAccessible(done);
};

module.exports = { expectUrlToBeAccessible };
