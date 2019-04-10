import { TestBed } from '@angular/core/testing';
import { FeatureFlagsService } from './feature-flags.service';

describe('FeatureFlagsService', () => {
  let subject;
  const name = 'foo';

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        FeatureFlagsService
      ]
    });

    localStorage.clear();
    subject = TestBed.get(FeatureFlagsService);
    subject._features = {};
  });

  it('populates _features from local storage keys that begin `featureFlags.undefined`', () => {
    localStorage.setItem(name, 'false');
    localStorage.setItem('featureFlags.undefined.bar', 'true');
    localStorage.setItem('featureFlags.undefined.baz', 'true');

    subject.loadLocalStorage();
    expect(Object.keys(subject._features).length).toBe(2);
  });

  it('coerces string values into booleans', () => {
    localStorage.setItem('featureFlags.undefined.bar', 'true');
    localStorage.setItem('featureFlags.undefined.baz', 'false');
    subject.loadLocalStorage();

    expect(subject.getFeatureStatus('bar')).toBe(true);
    expect(subject.getFeatureStatus('baz')).toBe(false);
  });

  it('treats undefined flags as not enabled', () => {
    expect(subject.getFeatureStatus('spaghetti')).toBe(false);
  });

  it('does not load localStorage keys outside of it\'s namespace', () => {
    localStorage.setItem(name, 'false');
    localStorage.setItem('featureFlags.undefined.bar', 'true');

    subject.loadLocalStorage();

    expect(Object.keys(subject._features).length).toBe(1);
    expect(subject._features[name]).toBeUndefined();
  });

  it('sets the feature flag in localStorage', () => {
    subject.setFeature(name, true);

    expect(subject._features[name]).toBe(true);
  });

  it('returns a feature flag status', () => {
    subject._features[name] = true;

    expect(subject.getFeatureStatus(name)).toBe(true);
  });

  it('serializes the keys of the feature object into localStorage', () => {
    subject._features[name] = true;
    subject._features['bar'] = false;
    subject._features['baz'] = true;

    subject.serialize();

    expect(Object.keys(localStorage).length).toEqual(3);
  });
});
