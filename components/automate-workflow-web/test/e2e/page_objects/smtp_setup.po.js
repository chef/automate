export default class SmtpSetupPage {

  get() {
    browser.get('#/smtp-setup');
  }

  get forbiddenText() {
    return element(by.css('.smtp-setup-forbidden'));
  }

  get smtpSetupFields() {
    return element(by.css('.smtp-setup-fields'))
  }

  get smtpHost() {
    return element(by.model('config.host'));
  }

  get smtpPort() {
    return element(by.model('config.port'));
  }

  get smtpLogin() {
    return element(by.model('config.smtp_login'));
  }

  get smtpPassword() {
    return element(by.model('config.password'));
  }

  get smtpSenderEmail() {
    return element(by.model('config.sender_email'));
  }

  get smtpSenderName() {
    return element(by.model('config.sender_name'));
  }

  get saveButton() {
    return element(by.buttonText('Save'));
  }

  get testConnectionButton() {
    return element(by.css('button.test-button'));
  }

  get smtpTestSuccessMessage() {
    return element(by.css('.smtp-test .result .success'));
  }

  get smtpTestUnsuccessfulMessage() {
    return element(by.css('.smtp-test .result .error'));
  }

  get removeButton() {
    return element(by.buttonText('Remove Configuration'));
  }

  get removeSmtpConfirmationModal() {
    return element(by.css('.remove-smtp-confirmation-modal'));
  }

  get configSaveSuccessFlash() {
    return element(by.css('.cd-flash.notify'));
  }

  get confirmDeleteButton() {
    return element(by.buttonText('Confirm'));
  }

  get cancelDeleteButton() {
    return element(by.buttonText('Cancel'));
  }
}
