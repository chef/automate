import ng from 'angular';
import 'angular-feature-flags/dist/featureFlags';

featureFlagsInitializer.$inject = [
  '$rootScope',
  '$document',
  'featureFlags'
];

// Make sure to add/remove the flags in the visibility-web/src/app/app.component.ts,
// as well as the unit and e2e tests
function featureFlagsInitializer($rootScope, $document, featureFlags) {
  let legacyFlags = [
    {key: 'legacy-compliance',
      name: "Legacy Compliance"}
  ];

  let betaFlags = [];

  let experimentalFlags = [
    {
      key: 'compliance-report',
      active: false,
      name: 'Compliance Report',
      description: 'View compliance reports when running inspec tests.'
    },
    {
      key: 'governance',
      active: false,
      name: 'Workflow Governance',
      description: 'Enable Teams functionality.'
    },
    {
      key: 'habitat-apps',
      active: false,
      name: 'Habitat Apps',
      description: 'View dashboard for managing your Habitat application deployments.'
    }
  ];

  let allFlags = betaFlags.concat(experimentalFlags);

  featureFlags.set(allFlags);

  function toggleFeatureFlags() {
    $rootScope.showFeatureFlags = !$rootScope.showFeatureFlags;
  }

  $rootScope.isOn = featureFlags.isOn;

  let keysCache = [];
  let codeLength = 4;
  let betaCode = [66, 69, 84, 65];
  let experimentalCode = [70, 69, 65, 84];
  let legacyCode = [76, 69, 71, 65];

  $document.on('keyup', event => {
    if (codeLength > keysCache.push(event.keyCode)) {
      return;
    }
    if (codeLength < keysCache.length) {
      keysCache.shift();
    }
    if (compareArray(betaCode, keysCache)) {
      featureFlags.set(betaFlags);
      $rootScope.$apply(toggleFeatureFlags);
    }
    if (compareArray(experimentalCode, keysCache)) {
      featureFlags.set(allFlags);
      $rootScope.$apply(toggleFeatureFlags);
    }
    if (compareArray(legacyCode, keysCache)) {
      featureFlags.set(legacyFlags);
      $rootScope.$apply(toggleFeatureFlags);
    }
  });

  function compareArray(a1, a2) {
    if (a1.length !== a2.length) return false;
    for (var i = 0; i < a1.length; i++) {
        if (a1[i] !== a2[i]) return false;
    }
    return true;
  }
}

export default ng
  .module('cd.common.featureFlags.initializer', ['feature-flags'])
  .run(featureFlagsInitializer)
  .name;
