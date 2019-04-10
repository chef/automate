export default class OrgsPage {

  get() {
    return browser.get('#/organizations');
  }

  get organizationsTab() {
    return element(by.linkText('Organizations'));
  }

  get workflowOrgsTab() {
    return element(by.linkText('Workflow Orgs'));
  }

  get defaultSearchTab() {
    return element(by.linkText('Default Search'));
  }

  get filterInput() {
    return element(by.model('orgFilter'));
  }

  get newOrgButton() {
    return element(by.css('[title="New Workflow Org"]'));
  }

  get newOrgForm() {
    return element(by.css('form[name="newOrgForm"]'));
  }

  get newOrgName() {
    return element(by.model('newOrg.name'));
  }

  get createOrgButton() {
    return element(by.buttonText('Save & Close'));
  }

  get newWebhookForm() {
    return element(by.css('form[name="newOrgForm"] .slack-webhook-form'));
  }

  get newWebhookName() {
    return element(by.model('webhook.name'));
  }

  get newWebhookUrl() {
    return element(by.model('webhook.url'));
  }

  get newWebhookNameField() {
    return element(by.css('.webhook-name.field'));
  }

  get newWebhookTestButton() {
    return element(by.buttonText('Send Test'));
  }

  get newWebhookTestSuccessMessage() {
    return element(by.css('.webhook-test .result .success'));
  }

  get newWebhookTestErrorMessage() {
    return element(by.css('.webhook-test .result .error'));
  }

  get editOrgButton() {
    return element.all(by.css('.org-card button[icon-button="pencil"]')).get(0);
  }

  get notificationTrashcan() {
    return element(by.css('.org-card button[icon-button="trash"]'));
  }

  get removeSlackConfirmationModal() {
    return element(by.css('.remove-slack-confirmation-modal'));
  }

  get orgsList() {
    return element.all(by.repeater('org in orgs'));
  }

  get orgsListNames() {
    return this
      .orgsList
      .map((item) => item.element(by.css('.card-title')).getText());
  }

  get sortAscBtn() {
    return element(by.css('.sort .ascending'));
  }

  get sortDscBtn() {
    return element(by.css('.sort .descending'));
  }

  get dashboardLink() {
    return element(by.css('[icon-button="chevrons"]'));
  }
}
