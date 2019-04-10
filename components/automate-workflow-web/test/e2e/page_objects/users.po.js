export default class UsersPage {

  get() {
    browser.get('#/users');
  }

  get filterInput() {
    return element(by.model('userFilter'));
  }

  get newUserButton() {
    return element(by.css('.card-add-btn'));
  }

  get newUserForm() {
    return element(by.css('form[name="newUserForm"]'));
  }

  get newUserFirst() {
    return this.newUserForm.element(by.model('user.first'));
  }

  get newUserLast() {
    return this.newUserForm.element(by.model('user.last'));
  }

  get newUserEmail() {
    return this.newUserForm.element(by.model('user.email'));
  }

  get newUserName() {
    return this.newUserForm.element(by.model('user.name'));
  }

  get newUserPassword() {
    return this.newUserForm.element(by.model('user.password'));
  }

  get createUserButton() {
    return element(by.buttonText('Save & Close'));
  }

  get cancelButton() {
    return element(by.buttonText('Cancel'));
  }

  get modalCancelButton() {
    return element(by.css('.cd-modal')).element(by.buttonText('Cancel'));
  }

  get editUserButton() {
    return element.all(by.css('.user-card button[icon-button="pencil"]')).get(0);
  }

  get usersList() {
    return element.all(by.repeater('user in users'));
  }

  get editUserForm() {
    return element(by.css('form.edit-user-form'));
  }

  get buttonBarUserType() {
    return element(by.css('form.edit-user-form .button-bar'));
  }

  get editUserPassword() {
    return element(by.model('user.password'));
  }

  get editUserFirst() {
    return element(by.model('user.first'));
  }

  get editUserLast() {
    return element(by.model('user.last'));
  }

  get editUserEmail() {
    return element(by.model('user.email'));
  }

  get internalUserButton() {
    return element(by.linkText('Internal'));
  }
  get ldapUserButton() {
    return element(by.linkText('LDAP'));
  }
  get samlUserButton() {
    return element(by.linkText('SAML'));
  }

  get userTypeChangeModal() {
    return element(by.css('.confirm-user-type-change-modal'));
  }

  get UsersListNames() {
    return this
      .UsersList
      .map((item) => item.element(by.css('.card-title')).getText());
  }

  get dashboardLink() {
    return element(by.css('[icon-button="chevrons"]'));
  }
}
