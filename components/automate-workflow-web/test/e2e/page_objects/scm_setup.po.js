export default class ScmSetupPage {

  get() {
    browser.get('#/scm-setup');
    browser.wait(presenceOf('.scm-setup'));
  }

  get title() {
    return element(by.css('.page-header h1'));
  }

  selectProvider(provider) {
    return element(by.buttonText(provider)).click();
  }

  get forbiddenText() {
    return element(by.css('.scm-setup-forbidden'));
  }

  get githubSetupFields() {
    return element(by.css('.github'));
  }

  get bitbucketSetupFields() {
    return element(by.css('.bitbucket-fields'));
  }

  get bitbucketUrlField() {
    return element(by.model('scmSetup.bitbucket.root_api_url'));
  }

  get bitbucketUserField() {
    return element(by.model('scmSetup.bitbucket.user_id'));
  }

  get bitbucketPassField() {
    return element(by.model('scmSetup.bitbucket.password'));
  }

  get saveButton() {
    return element(by.buttonText('Save'));
  }

  get updateButton() {
    return element(by.buttonText('Save'));
  }

  get removeButton() {
    return element(by.buttonText('Remove Configuration'));
  }

  get confirmationModal() {
    return element(by.css('.cd-modal-overlay'));
  }

  get modalCancelButton() {
    return element(by.buttonText('Cancel'));
  }

  get modalConfirmButton() {
    return element(by.buttonText('Confirm'));
  }
}
