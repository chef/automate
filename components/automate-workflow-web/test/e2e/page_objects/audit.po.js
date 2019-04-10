export default class AuditPage {

  get() {
    browser.get('#/audit');
  }

  get auditLog() {
    return element(by.css('.audit-log'));
  }

  get auditLogItems() {
    return element.all(by.css('.audit-log-item'));
  }

  get projectLinks() {
    return element.all(by.binding('audit.proj'));
  }

  get changeLinks() {
    return element.all(by.binding('audit.change_title'));
  }

  get actionItems() {
    return element.all(by.binding('audit.action'));
  }

  get stageItems() {
    return element.all(by.binding('audit.stage_name'));
  }

  get statusItems() {
    return element.all(by.css('[data-field="status"] span'));
  }

  get submittedByItems() {
    return element.all(by.binding('audit.submitted_by'));
  }

  get approvedByItems() {
    return element.all(by.binding('audit.approved_by'));
  }

  get deliveredByItems() {
    return element.all(by.binding('audit.delivered_by'));
  }

  get filterInput() {
    return element(by.model('filterText'));
  }
}
