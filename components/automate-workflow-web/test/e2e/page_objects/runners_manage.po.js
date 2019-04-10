export default class RunnersManagePage {

  get() {
    browser.get('#/runners/manage');
  }

  get noRunnersInfo() {
    return element(by.css('.no-runners-info'));
  }

  get testButtons() {
    return element.all(by.css('.runner-check'));
  }

  get statusIcons() {
    return element.all(by.css('.runner-status-icon'));
  }

  get runnerHostnames() {
    return element.all(by.binding('runner.hostname'));
  }

  get runnerJobs() {
    return element.all(by.css('.runner-job'));
  }

  get runnerIdles() {
    return element.all(by.css('.runner-idle'));
  }

  get runnerOutputs() {
    return element.all(by.css('.health-output .field-tooltip'));
  }
}
