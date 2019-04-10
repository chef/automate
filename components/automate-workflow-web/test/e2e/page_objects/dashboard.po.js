export default class Dashboard {

  constructor() {
    this.clearStore();
  }

  get() {
    browser.get('#/dashboard');
  }

  clearStore() {
    browser.executeScript('window.sessionStorage.clear();');
  }

  get projects() {
    return element.all(by.css('.project-item'));
  }

  get projectChanges() {
    return element.all(by.css('.project-items .change-item'));
  }

  get includedChanges() {
    return element.all(by.css('.included .change-item'));
  }

  get filterInput() {
    return element(by.model('filterText'));
  }

  get clearButton() {
    return element(by.css('.filter button'));
  }

  ribbonStage(index) {
    return element.all(by.css('.ribbon .stage')).get(index);
  }

  get approvables() {
    return this.ribbonStage(1).element(by.css('.status'));
  }

  get ribbonHumanIcon() {
    return element(by.css('.stages .waiting'));
  }

  get projectHumanIcon() {
    return element(by.css('.project-item .waiting'));
  }

  get changeHumanIcon() {
    return element(by.css('.change-item .waiting'));
  }

  get firstProject() {
    return element.all(by.css('.project-item')).first();
  }
}
