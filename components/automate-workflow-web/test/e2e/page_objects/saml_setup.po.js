export default class SamlSetupPage {

  get() {
    browser.get('#/saml-setup');
  }

  get forbiddenText() {
    return element(by.css('.saml-setup-forbidden'));
  }

  get samlSetupFields() {
    return element(by.css('.saml-setup-fields'))
  }

  get samlManualCheckbox() {
    return element(by.css('#manual-checkbox'));
  }

  get samlMetadataCheckbox() {
    return element(by.css('#metadata-checkbox'));
  }

  get samlMetadataUrl() {
    return element(by.model('config.metadata_url'));
  }

  get samlSSOLogin() {
    return element(by.model('config.sso_login_url'));
  }

  get samlIdPUrl() {
    return element(by.model('config.idp_url'));
  }

  get samlNameIdPolicy() {
    return element(by.model('config.name_id'));
  }

  get samlCert() {
    return element(by.model('config.cert'));
  }

  get samlDefaultRoles() {
    return element(by.css('.roles'));
  }

  get saveButton() {
    return element(by.buttonText('Save'));
  }

  get overwriteSamlConfirmationModal() {
    return element(by.css('.delete-confirmation-modal'));
  }

  get confirmEditButton() {
    return element(by.buttonText('Confirm'));
  }

  get cancelEditButton() {
    return element(by.buttonText('Cancel'));
  }
}
