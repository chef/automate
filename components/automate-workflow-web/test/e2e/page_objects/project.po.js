export default class ProjectPage {

  get(params) {
    let path = `#/organizations/${params.org}/projects/${params.project}`;
    browser.get(path);
  }

  get pipelinesTab() {
    return element(by.css('.tab.pipelines'));
  }

  get cloneTab() {
    return element(by.css('.tab.clone'));
  }

  get pipelineCreationForm() {
    return element(by.css('.pipes-container form'));
  }

  get pipelineCreationInstructions() {
    return element(by.css('.pipes-container .no-items'));
  }

  get dependenciesTab() {
    return element(by.css('.tab.dependencies'));
  }

  get dependenciesTabCount() {
    return element(by.css('.tab.dependencies .badge'));
  }

  get dependencyList() {
   return element.all(by.css('.dependency .dep-card'));
  }

  get consumersTab() {
    return element(by.css('.tab.consumers'));
  }

  get requiredByTabCount() {
    return element(by.css('.tab.consumers .badge'));
  }

  get requiredByList() {
   return element.all(by.css('.consumer .dep-card'));
  }

  get projectName() {
    return element(by.css('.project-name'));
  }

  get cloneOptions() {
    return element(by.css('.project-repo'));
  }

  get watchButton() {
    return element(by.css('.watch-button button'));
  }

  get watchMenu() {
    return element(by.css('.watch-menu'));
  }

  get watchCategories() {
    return element.all(by.css('.watch-menu .categories li label'));
  }

  categoryCheckbox(categoryName) {
    return element(by.css('.watch-menu .categories input[value="' + categoryName + '"]'))
  }
}
