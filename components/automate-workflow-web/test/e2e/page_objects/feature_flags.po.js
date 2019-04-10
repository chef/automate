export default class FeatureFlags {

  get overlay() {
    return element(by.css('.feature-flags'));
  }

  get flagListItems() {
    return element.all(by.css('.feature-flags-flag'));
  }

  toggleBetaVisibility() {
    element(by.css('body')).sendKeys('beta');
  }

  toggleLegacyVisibility() {
    element(by.css('body')).sendKeys('lega');
  }

  toggleSecretVisibility() {
    element(by.css('body')).sendKeys('feat');
  }

  enable(flag) {
    element(by.id(flag)).element(by.buttonText('On')).click();
  }

  disable(flag) {
    element(by.id(flag)).element(by.buttonText('Off')).click();
  }
}
