var enableWelcomeModal = function() {
  browser.executeScript('return window.sessionStorage.clear();');
  browser.executeScript('return window.localStorage.clear();');
}

// There is a security feature in Chrome that prevents accessing localStorage or
// sessionStorage before a page has been loaded. To avoid having to remember to
// disable the welcome modal in every test, this function can be used in a
// before hook to visit a page and let the server set things up such that the
// welcome modal won't appear on subsequent page loads.
var disableWelcomeModal = function(done) {
  browser.get('/404').then(() => {
    // Visiting a URL before the fake server is setup spawns some potentially
    // confusing warnings that I couldn't figure out how to turn off.
    console.log('🙈  The HPM proxy errors above this line can be ignored.');
    // Flush the browser's logs.
    browser.manage().logs().get('browser');
    done();
  });
}

module.exports = {};
module.exports.enableWelcomeModal = enableWelcomeModal;
module.exports.disableWelcomeModal = disableWelcomeModal;
