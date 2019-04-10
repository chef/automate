export default class QuickFind {

  get input() {
    return element(by.css('.quick-find-input'));
  }

  get results() {
    return this.resultsDropdown.all(by.css('li'));
  }

  get resultsDropdown() {
    return element(by.css('[cd-quick-find] .dropdown-menu'));
  }
}
