export default function featureFlag(action, key) {
  let rootScope;

  // Note: These have to match the order of the feature keys
  // listed in common/feature_flags/feature_flags.js
  // (or wherever they're coming from to drive the content
  // for angular-feature-flags) because we're toggling them
  // based on position -- at least until we can refer to them
  // more directly (e.g., by class name), which the directive
  // doesn't seem to expose yet in its API. Sigh.
  let flagIndex = ['compliance', 'compliance-report', 'governance'].indexOf(key);

  browser.executeScript(() => {
    rootScope = angular.element(document).scope().$root;
  });

  toggleMenu(true);
  setFlag(action, key);
  toggleMenu(false);

  // Normally we would actually click on the popup to enable/disable the
  // feature flag but could never get that working correctly. To get
  // tests passing we are setting the value in the scope that would be
  // set by clicking on the popup.
  function setFlag(action, key) {
    browser.executeScript((action, flagIndex) => {
      let flagElement = document.querySelectorAll('.feature-flags-flag')[flagIndex];
      let flagScope = angular.element(flagElement).scope();
      let flag = flagScope.flag;

      flagScope.$apply(function() {
        flagScope[action](flag);
      });
    }, action, flagIndex);
  }

  // Normally we would actually send 'ctrl+r' to the page to show/hide the
  // feature flag popup but could never get that working correctly. To get
  // tests passing we are toggling the value in the scope that would be
  // toggled by sending the hotkeys.
  function toggleMenu(show) {
    browser.executeScript(() => {
      rootScope.$apply(() => {
        rootScope.showFeatureFlags = show;
      });
    });
  }
}

global.featureFlag = featureFlag;
