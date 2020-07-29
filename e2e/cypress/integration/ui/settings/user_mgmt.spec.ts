describe('user management', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const typeDelay = 50;
  const cypressPrefix = 'user-mgmt';
  const name = `cypress test user ${now}`;
  const username = `${cypressPrefix}-test-${now}`;
  const password = 'testing!';

  before(() => {
    cy.adminLogin('/settings/users').then(() => {
      cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users']);
    });
    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });
  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users']);
  });

  it('can create a user', () => {
    cy.get('app-user-table').should('exist');

    // local admin user should always exist
    cy.get('.username-column').contains('admin').should('exist');

    // open modal
    cy.get('[data-cy=app-user-table-add-button]').click();
    cy.get('app-user-management chef-modal').should('exist');

    // we increase the default delay to mimic the average human's typing speed
    // only need this for input values upon which later test assertions depend
    // ref: https://github.com/cypress-io/cypress/issues/534
    cy.get('[formcontrolname=displayName]').focus()
      .type(name, { delay: typeDelay }).should('have.value', name);

    // username is auto-generated from the display name
    cy.get('#username-fields span').contains(`cypress-test-user-${now}`).should('exist');

    // we can also edit the username directly
    cy.get('[data-cy=edit-username]').click();
    cy.get('[formcontrolname=username]').focus().clear()
      .type(username, { delay: typeDelay }).should('have.value', username);

    // type a too short password
    cy.get('[formcontrolname=password]')
      .type('1234567', { delay: typeDelay }).should('have.value', '1234567');

    // focus on a different form field to trigger password error
    cy.get('[formcontrolname=confirmPassword]').focus();
    cy.get('app-create-user-modal chef-error').contains('must be at least')
      .should('be.visible');

    // type a valid password
    cy.get('[formcontrolname=password]').clear()
      .type(password, { delay: typeDelay }).should('have.value', password);

    // type a mismatch confirmPassword
    cy.get('[formcontrolname=confirmPassword]')
      .type('different', { delay: typeDelay }).should('have.value', 'different');

    // focus on a different form field to trigger confirmPassword error
    cy.get('[formcontrolname=password]').focus();
    cy.get('app-create-user-modal chef-error').contains('must match')
      .should('be.visible');

    // type a matching confirmPassword
    cy.get('[formcontrolname=confirmPassword]').clear()
      .type(password, { delay: typeDelay }).should('have.value', password);

    // save new user
    cy.get('[data-cy=save-user]').click();
    cy.get('app-user-management chef-modal').should('not.be.visible');
    cy.get('app-notification.info').contains(`Created user ${username}`);
    // close notification banner so we can assert the password success notification later
    cy.get('app-notification.info chef-icon').click();

    cy.get('app-user-table chef-td').contains(username).should('exist');
    cy.get('app-user-table chef-td').contains(name).should('exist');
  });

  it('can view and edit user details', () => {
    cy.route('GET', `**/users/${username}`).as('getUser');
    cy.route('PUT', `**/users/${username}`).as('updateUser');

    const updated_name = `${name} updated`;
    const updated_password = 'testing?';

    cy.contains(name).click();
    cy.wait('@getUser');

    cy.get('app-user-details div.name-column').contains(name).should('exist');
    cy.get('app-user-details div.header-user-id').contains(username).should('exist');
    cy.get('app-user-details chef-form-field').contains('Display Name ').should('exist');
    cy.get('[formcontrolname=displayName]').clear()
      .type(updated_name, { delay: typeDelay }).should('have.value', updated_name);

    // save display name change
    cy.get('[data-cy=user-details-submit-button]').click();
    cy.get('app-user-details span#saved-note').contains('All changes saved.').should('exist');
    cy.get('app-user-details div.name-column').contains(updated_name).should('exist');

    // click password tab
    cy.get('app-user-details #chef-option2').click();
    cy.get('[formcontrolname=newPassword]')
      .type(updated_password, { delay: typeDelay }).should('have.value', updated_password);

    // type a mismatch confirmPassword
    cy.get('[formcontrolname=confirmPassword]')
      .type(password, { delay: typeDelay }).should('have.value', password);

    cy.get('app-user-details .password:last-of-type chef-error').contains('must match')
      .should('be.visible');

    // type a matching confirmPassword
    cy.get('[formcontrolname=confirmPassword]').clear()
      .type(updated_password, { delay: typeDelay }).should('have.value', updated_password);

    cy.get('.update-password-button').click();
    cy.get('#main-content-wrapper').scrollTo('top');

    cy.wait('@updateUser');
    cy.get('app-notification.info').contains(`Reset password for user: ${username}`);
  });
});
