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
    cy.route('POST', '**/users').as('createUser');
    cy.get('app-user-table').should('exist');

    // open modal
    cy.get('app-user-table chef-button').contains('Create User').as('createUserBtn')
      // this line retries until the element is detected as visible by Cypress
      // ref: https://github.com/cypress-io/cypress/issues/695
      .invoke('width').should('be.greaterThan', 0)
    cy.get('@createUserBtn').click();
    cy.get('app-user-management chef-modal').should('exist');

    // we increase the default delay to mimic the average human's typing speed
    // only need this for input values upon which later test assertions depend
    // ref: https://github.com/cypress-io/cypress/issues/534
    cy.get('[formcontrolname=fullname]').focus()
      .type(name, { delay: typeDelay }).should('have.value', name);

    cy.get('[formcontrolname=username]').focus()
      .type(username, { delay: typeDelay }).should('have.value', username);

    cy.get('[formcontrolname=password]').focus()
      .type(password, { delay: typeDelay }).should('have.value', password);

    cy.get('[formcontrolname=confirmPassword]')
      .type(password, { delay: typeDelay }).should('have.value', password);

    // save new user
    cy.get('[data-cy=save-user]').click();
    cy.get('app-user-management chef-modal').should('not.be.visible');
    cy.wait('@createUser');

    cy.get('chef-notification.info').contains('created a new user')
      .invoke('width').should('be.greaterThan', 0);
    cy.get('app-user-table chef-td').contains(username).should('exist');
    cy.get('app-user-table chef-td').contains(name).should('exist');
  });

  it.skip('can view and edit user details', () => {
    cy.route('GET', `**/users/${username}`).as('getUser');
    cy.route('PUT', `**/users/${username}`).as('updateUser');

    const updatedName = `${name} updated`;
    const updatedPassword = 'testing?';

    cy.contains(name).click();
    cy.wait('@getUser');

    cy.get('app-user-details div.name-column').contains(name).should('exist');
    cy.get('app-user-details span').contains(username).should('exist');
    cy.get('app-user-details chef-form-field').contains('New Password').should('exist');
    cy.get('app-user-details chef-form-field').contains('Confirm New Password').should('exist');

    cy.get('app-user-details chef-button.edit-button').click();

    cy.get('[formcontrolname=fullName]').should('not.be.disabled')
      .invoke('val', updatedName).trigger('input');

    cy.get('app-user-details chef-button.save-button').click();
    cy.wait('@updateUser');
    cy.get('chef-notification.info').contains('updated user')
      .invoke('width').should('be.greaterThan', 0);

    // use this method of inputing data into form to simulate copy-pasting
    // to ensure we get the same value in both form inputs
    cy.get('[formcontrolname=newPassword]')
      .invoke('val', updatedPassword).trigger('input');
    cy.get('[formcontrolname=confirmPassword]')
      .invoke('val', updatedPassword).trigger('input');

    cy.get('app-user-details chef-button').contains('Update Password').click({ force: true} );
    cy.wait('@updateUser');

    cy.get('chef-notification.info').contains('updated user')
      .invoke('width').should('be.greaterThan', 0);
  });

  it.skip('can delete user', () => {
    cy.route('GET', '**/users').as('getUsers');
    cy.route('DELETE', `**/users/${username}`).as('deleteUser');

    // back to user list page
    cy.get('app-user-details .breadcrumb').contains('Users').click();
    cy.wait('@getUsers');

    cy.get('app-user-table chef-td').contains(username).parent()
        .find('chef-control-menu').as('controlMenu')
        // we throw in a should so cypress waits until introspection allows menu to be shown
        .invoke('width').should('be.greaterThan', 0);
    cy.get('@controlMenu').click();
    cy.get('@controlMenu').find('[data-cy=delete]').click({ force: true });

    // confirm in modal
    cy.get('app-user-management chef-button').contains('Delete User').click();

    cy.wait('@deleteUser');
    cy.get('app-user-management chef-tbody chef-td').contains(username).should('not.exist');
  });
});
