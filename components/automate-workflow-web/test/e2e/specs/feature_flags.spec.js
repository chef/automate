import FeatureFlags from '../page_objects/feature_flags.po';

describe('feature flags', () => {

  let featureFlags;

  beforeAll(login);

  afterAll(logout);

  beforeEach(() => {
    featureFlags = new FeatureFlags();
  });

  describe('overlay', () => {

    it('displays overlay when toggled', () => {
      featureFlags.toggleBetaVisibility();
      expect(featureFlags.overlay).toBeDisplayed();
      featureFlags.toggleBetaVisibility();
    });

    it('displays a list of beta feature flags', () => {
      featureFlags.toggleBetaVisibility();

      expect(featureFlags.flagListItems.count()).toEqual(0);
      featureFlags.flagListItems.each((flagListItem) => {
        expect(flagListItem).toBeDisplayed();
      });

      featureFlags.toggleBetaVisibility();
    });
  });

  describe('overlay', () => {

    it('displays overlay when toggled', () => {
      featureFlags.toggleLegacyVisibility();
      expect(featureFlags.overlay).toBeDisplayed();
      featureFlags.toggleLegacyVisibility();
    });

    it('displays a list of Legacy feature flags', () => {
      featureFlags.toggleLegacyVisibility();

      expect(featureFlags.flagListItems.count()).toEqual(1);
      featureFlags.flagListItems.each((flagListItem) => {
        expect(flagListItem).toBeDisplayed();
      });

      featureFlags.toggleLegacyVisibility();
    });
  });

  describe('top-secret premium deluxe feature flags overlay', () => {

    it('displays overlay when toggled', () => {
      featureFlags.toggleSecretVisibility();
      expect(featureFlags.overlay).toBeDisplayed();
      featureFlags.toggleSecretVisibility();
    });

    it('displays a list of beta and secret feature flags', () => {
      featureFlags.toggleSecretVisibility();

      expect(featureFlags.flagListItems.count()).toEqual(3);
      featureFlags.flagListItems.each((flagListItem) => {
        expect(flagListItem).toBeDisplayed();
      });

      featureFlags.toggleSecretVisibility();
    });
  });
});
