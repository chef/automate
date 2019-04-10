export default class ChangePage {

  get(params) {
    let path = `#/organizations/${params.org}/projects/${params.project}/changes/${params.change}`;
    browser.get(path);

    return browser.wait(presenceOf('.change-title'));
  }

  get title() {
    return element(by.css('.change-title'));
  }

  get summaryTab() {
    return element.all(by.css('.tabs .tab')).first();
  }

  get description() {
    return element(by.css('.change-description'));
  }

  get commitItems() {
    return element.all(by.css('.commit-item'));
  }

  get deliveredBy() {
    return element(by.css('.delivered-by'));
  }

  get approveButton() {
    return element(by.css('.button.approve'));
  }

  get deliverButton() {
    return element(by.css('.button.deliver'));
  }

  get promotionStatus() {
    return element(by.css('.promotion-status'));
  }

  get supersedingChangeLink() {
    return element(by.css('.superseding-change-link'));
  }

  get unreachableStages() {
    return element.all(by.css('.status-stage.unreachable'));
  }

  get patchsetStatus() {
    return element(by.css('[cd-patchset-status]'));
  }

  get patchsetStatusContainer() {
    return element(by.css('.patchset-status'));
  }

  get stageRunDetails() {
    return element(by.css('.stage-run-details'));
  }

  get phaseRunDetailDescription() {
    return element.all(by.css('.phase-list li:first-child .phase-list li:first-child')).first();
  }

  get logDisplay() {
    return element(by.css('.log-display'));
  }

  get statusNavHumanIcon() {
    return element(by.css('.status-nav .waiting'));
  }

  get stageTitleHumanIcon() {
    return element(by.css('.stage-title .waiting'));
  }

  get externalPrLink() {
    return element(by.css('.external-pr'));
  }

  logIsDisplayed() {
    return element(by.css('.log-display'))
      .getText()
      .then((text) => text.length > 0);
  }

  get complianceReport() {
    return element(by.css('.compliance'));
  }

  get switchViewsButton() {
    return element(by.css('.btn-switch'));
  }

  get downloadButton() {
    return element(by.css('.log-download'));
  }

  openPhaseLog() {
    let phaseItem = element.all(
      by.css('.phase-list li:first-child .phase-details')).last();

    phaseItem.click();
    return browser.wait(this.logIsDisplayed());
  }

  openRunDetailPhaseLog() {
    let runDetailItem = element.all(by.css('.phase-list li:first-child .phase-list li:first-child')).first();

    runDetailItem.click();
    return browser.wait(this.logIsDisplayed());
  }

  clickCloseButton() {
    let closeButton = element(by.css('.icon-close'));
    closeButton.click();
  }

  openNestedPhases() {
    let test = element.all(
      by.css('.phase-list li:first-child .phase-details')).first();

    test.click();
  }

  scrollUp() {
    let button = element(by.css('.btn-top'));
    button.click();
  }

  scrollDown() {
    let button = element(by.css('.btn-bottom'));
    button.click();
  }
}
