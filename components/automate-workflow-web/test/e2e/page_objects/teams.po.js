export default class TeamsPage {

  get() {
    browser.get('#/teams');
  }

  get newTeamButton() {
    return element(by.css('.card-add-btn'));
  }

  get newTeamForm() {
    return element(by.css('.new-team-form'));
  }

  get newTeamName() {
    return this.newTeamForm.element(by.model('team.name'));
  }

  get newTeamDescription() {
    return this.newTeamForm.element(by.model('team.description'));
  }

  get createTeamButton() {
    return element(by.buttonText('Save'));
  }

  get cancelButton() {
    return element(by.buttonText('Cancel'));
  }

  get modalCancelButton() {
    return element(by.css('.cd-modal')).element(by.buttonText('Cancel'));
  }

  get teamsList() {
    return element.all(by.css('.card-content'));
  }

  teamEntry(name) {
    return element(by.linkText(name));
  }

  get dashboardLink() {
    return element(by.css('[icon-button="chevrons"]'));
  }

  get newTeamBreadcrumbNotLinked() {
    return element(by.css('.teams-page-not-linked'));
  }

  get newTeamBreadcrumbLinked() {
    return element(by.css('.teams-page-linked'));
  }

  get newTeamBreadcrumb() {
    return element(by.css('.new-team'));
  }
}
