describe('user management', () => {
  before(() => {
    cy.adminLogin('/settings/users').then(() => {

      // clean up leftover users in case of previous test failures
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cy.cleanupUsersByNamePrefix(admin.id_token, 'cypress test user ');
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });
  afterEach(() => {
    cy.saveStorage();
  });

  const now = Cypress.moment().format('MMDDYYhhmm');
  const typeDelay = 50;
  const name = `cypress test user ${now}`;
  const username = `testing${now}`;
  const password = 'testing!';

  it('can create a user', () => {
    cy.get('app-user-table').should('exist');

    // open modal
    cy.get('app-user-table chef-button').contains('Create User').click();
    cy.get('app-user-management chef-modal').should('exist');

    // adding a delay to more closely approximate an average human's type speed (200 char/min)
    // the `should` will wait until the actual input value matches the expected value
    // ref: https://github.com/cypress-io/cypress/issues/534
    cy.get('[formcontrolname=fullname]')
      .type(name, { delay: typeDelay }).should('have.value', name);

    cy.get('[formcontrolname=username]')
      .type(username, { delay: typeDelay }).should('have.value', username);

    cy.get('[formcontrolname=password]')
      .type(password, { delay: typeDelay }).should('have.value', password);

    cy.get('[formcontrolname=confirmPassword]')
      .type(password, { delay: typeDelay }).should('have.value', password);

    // save new user
    cy.get('[data-cy=save-user]').click();
    cy.get('app-user-management chef-modal').should('not.be.visible');
    cy.get('chef-notification.info').should('be.visible');

    cy.get('app-user-table chef-td').contains(username).should('exist');
    cy.get('app-user-table chef-td').contains(name).should('exist');
  });

  it('can view and edit user details', () => {
    cy.route('PUT', '**/users/**').as('updateUser');

    const updated_name = `${name} updated`;
    const updated_password = 'testing?';
    cy.contains(name).click();
    cy.get('app-user-details div.name-column').contains(name).should('exist');
    cy.get('app-user-details span').contains(username).should('exist');
    cy.get('app-user-details chef-form-field').contains('New Password').should('exist');
    cy.get('app-user-details chef-form-field').contains('Confirm New Password').should('exist');

    cy.get('app-user-details chef-button.edit-button').click();
    cy.get('[formcontrolname=fullName]').find('input')
      .clear().type(updated_name, { delay: typeDelay }).should('have.value', updated_name);

    cy.get('app-user-details chef-button.save-button').click();
    cy.wait('@updateUser');
    cy.contains(updated_name).should('exist');

    cy.get('[formcontrolname=newPassword]').find('input')
      .type(updated_password, { delay: typeDelay }).should('have.value', updated_password);
    cy.get('[formcontrolname=confirmPassword]').find('input')
      .type(updated_password, { delay: typeDelay }).should('have.value', updated_password);
    cy.get('app-user-details chef-button').contains('Update Password').click({ force: true} );

    // success alert displays
    cy.get('chef-notification.info').should('be.visible');

    // back to user list page
    cy.get('app-user-details .breadcrumb').contains('Users').click();
  });

  it('can delete user', () => {
    cy.route('DELETE', '**/users/**').as('deleteUser');

    cy.get('app-user-table chef-td').contains(name).parent().parent()
        .find('chef-control-menu').as('controlMenu');
    // we throw in a should so cypress waits until introspection allows menu to be shown
    cy.get('@controlMenu').should('be.visible')
      .click();
    cy.get('@controlMenu').find('[data-cy=delete]').click({ force: true });

    // confirm in modal
    cy.get('app-user-management chef-button').contains('Delete User').click();

    cy.wait('@deleteUser');
    cy.get('app-user-management chef-tbody chef-td').contains(username).should('not.exist');
  });
});
