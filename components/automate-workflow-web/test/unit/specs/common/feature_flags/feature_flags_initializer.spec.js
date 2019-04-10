import ng from 'angular';
import key from 'keymaster';
import 'angular-mocks';
import featureFlagsInitializer
  from '../../../../../src/common/feature_flags/feature_flags_initializer';

describe('featureFlagsInitializer', () => {

  beforeEach(ng.mock.module(featureFlagsInitializer, ($provide) => {
    $provide.value('featureFlags', {
      set: jasmine.createSpy()
    });
  }));

  it('sets the available feature flags', inject((featureFlags) => {
    expect(featureFlags.set).toHaveBeenCalledWith([
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
    ]);
  }));

  it('attaches the `isOn` method to rootScope', inject((featureFlags, $rootScope) => {
    expect($rootScope.isOn).toEqual(featureFlags.isOn);
  }));

  it('sets keyboard shortcut to toggle feature flags overlay', inject(($document, $rootScope) => {
    // f+e+a+t
    let fKeyup = Object.assign(new Event('keyup'), { keyCode: 70 });
    let eKeyup = Object.assign(new Event('keyup'), { keyCode: 69 });
    let aKeyup = Object.assign(new Event('keyup'), { keyCode: 65 });
    let tKeyup = Object.assign(new Event('keyup'), { keyCode: 84 });

    $rootScope.showFeatureFlags = false;
    $document[0].dispatchEvent(fKeyup);
    $document[0].dispatchEvent(eKeyup);
    $document[0].dispatchEvent(aKeyup);
    $document[0].dispatchEvent(tKeyup);

    expect($rootScope.showFeatureFlags).toBe(true);
  }));

  it('sets keyboard shortcut to toggle secret feature flags overlay', inject(($document, $rootScope) => {
    // b+e+t+a
    let bKeyup = Object.assign(new Event('keyup'), { keyCode: 66 });
    let eKeyup = Object.assign(new Event('keyup'), { keyCode: 69 });
    let tKeyup = Object.assign(new Event('keyup'), { keyCode: 84 });
    let aKeyup = Object.assign(new Event('keyup'), { keyCode: 65 });

    $rootScope.showFeatureFlags = false;
    $document[0].dispatchEvent(bKeyup);
    $document[0].dispatchEvent(eKeyup);
    $document[0].dispatchEvent(tKeyup);
    $document[0].dispatchEvent(aKeyup);

    expect($rootScope.showFeatureFlags).toBe(true);
  }));
});
