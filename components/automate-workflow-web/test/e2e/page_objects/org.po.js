export default class OrgPage {

  get(params) {
    let path = `#/organizations/${params.org}`;
    browser.get(path);
  }

  get newProjectButton() {
    return element(by.css('.card-add-btn'));
  }

  get scmFieldset() {
    return element(by.css('[cd-project-repo]'));
  }

  get scmOptions() {
    return element(by.css('.new-project .button-bar')).all(by.css('.button'));
  }

  scmOption(name) {
    return element(by.linkText(name));
  }

  get newProjectForm() {
    return element(by.css('form.new-project'));
  }

  get projectFilter() {
    return element(by.css('.card-filter'));
  }

  get projectCards() {
    return element(by.css('.cards')).all(by.css('.project-card'));
  }

  get projectNameField() {
    return element(by.model('newProject.name'));
  }

  get bitbucketFields() {
    return element(by.css('.bitbucket'));
  }

  get bitbucketInstructions() {
    return element(by.css('.project-scm-instructions'));
  }

  get bitbucketProjectKeyField() {
    return element(by.model('project.scm.project_key'));
  }

  get bitbucketRepositoryNameField() {
    return element(by.model('project.scm.repo_name'));
  }

  get bitbucketPipelineBranchField() {
    return element(by.model('project.scm.pipeline_branch'));
  }

  get githubFields() {
    return element(by.css('.github'));
  }

  get saveButton() {
    return element(by.buttonText('Save & Close'));
  }

  get editProjectSCMProjectKey() {
    return element(by.model('editProject.scm.projectKey'));
  }

  get editProjectSCMRepoOwner() {
    return element(by.model('editProject.scm.repoOwner'));
  }

  get newWebhookForm() {
    return element(by.css('form.new-project .slack-webhook-form'));
  }

  get newWebhookUrl() {
    return element(by.css('form.new-project input[ng-model="webhook.url"]'));
  }

  get newWebhookNameField() {
    return element(by.css('form.new-project .webhook-name.field'));
  }

  get newWebhookName() {
    return element(by.css('form.new-project input[ng-model="webhook.name"]'));
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

  get editProjectForm() {
    return element(by.css('form.edit-project-form'));
  }

  get editWebhookForm() {
    return element(by.css('form.edit-project-form .slack-webhook-form'));
  }

  get editWebhookUrl() {
    return element(by.css('form.edit-project-form input[ng-model="webhook.url"]'));
  }

  get editWebhookNameField() {
    return element(by.css('form.edit-project-form .webhook-name.field'));
  }

  get editWebhookName() {
    return element(by.css('form.edit-project-form input[ng-model="webhook.name"]'));
  }

  get editWebhookTestButton() {
    return element(by.buttonText('Send Test'));
  }

  get editWebhookTestSuccessMessage() {
    return element(by.css('.webhook-test .result .success'));
  }

  get editWebhookTestErrorMessage() {
    return element(by.css('.webhook-test .result .error'));
  }

  get notificationTrashcan() {
    return element(by.css('form.edit-project-form button[icon-button="trash"]'));
  }

  get removeSlackConfirmationModal() {
    return element(by.css('.remove-slack-confirmation-modal'));
  }
}
