const ignoredMessages = [
  // If an HTTP request is made to a URL that has not been stubbed but is
  // proxied to hock, hock will log a warning and respond with a 500. If a
  // request is made to a URL that is not proxied to hock the browser logs a
  // 404 error, so that will show up in the test output.
  /Failed to load resource: the server responded with a status of 500/,
  // Some of the HTTP errors in the browser logs are accompanied with cryptic
  // messages like this:
  // http://localhost:49152/main.bundle.js 202657:18 "ERROR"
  /main\.bundle\.js .* "ERROR"/,
  // A deprecation warning from Chrome with this URL given for more context:
  // https://goo.gl/EGXzpw
  /deprecation - Styling master document from stylesheets defined in HTML Imports is deprecated/,
]

// The browser log entries returned have the layout found here:
// https://github.com/SeleniumHQ/selenium/wiki/JsonWireProtocol#log-entry-json-object
var displayBrowserLogs = function() {
  browser.manage().logs().get('browser').then((entries) => {
    filtered_entries = entries.filter((entry) => {
      return !ignoredMessages.some((regex) => entry.message.match(regex));
    });

    if(filtered_entries.length > 0) {
      console.log('');
      console.log('====================');
      console.log('Browser console messages:');
      console.log('');
      filtered_entries.forEach((entry) => {
        console.log(entry.level.name + ': ' + entry.message)
      });
      console.log('====================');
      console.log('');
    };
  });
};

module.exports = {};
module.exports.displayBrowserLogs = displayBrowserLogs;
