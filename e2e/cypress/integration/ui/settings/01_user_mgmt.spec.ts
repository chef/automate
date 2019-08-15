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
  const name = `cypress test user ${now}`;
  const username = `testing${now}`;

  it('can create a user', () => {
    cy.get('app-user-table').should('exist');

    // open modal
    cy.get('app-user-table chef-button').contains('Create User').click();
    cy.get('app-user-management chef-modal').should('exist');

    cy.get('[formcontrolname=fullname]')
      .focus().type(name);

    cy.get('[formcontrolname=username]')
      .focus().type(username);

    cy.get('[formcontrolname=password]')
      .focus().type('chefautomate');

    cy.get('[formcontrolname=confirmPassword]')
      .focus().type('chefautomate');

    // save new user
    cy.get('[data-cy=save-user]').click();
    cy.get('app-user-management chef-modal').should('not.be.visible');
    cy.get('chef-notification.info').should('be.visible');

    cy.get('chef-td').contains(username).should('exist');
    cy.get('chef-td').contains(name).should('exist');
  });

  it('can view and edit user details', () => {
    cy.route('PUT', '**/users/**').as('updateUser');

    const updated_name = `${name} updated`;
    const updated_password = 'chefautomate1';
    cy.contains(name).click();
    cy.get('app-user-details').should('exist');
    cy.get('div.name-column').contains(name).should('exist');
    cy.get('span').contains(username).should('exist');
    cy.get('chef-form-field').contains('New Password').should('exist');
    cy.get('chef-form-field').contains('Confirm New Password').should('exist');

    cy.get('chef-button.edit-button').click();
    cy.get('[formcontrolname=fullName]').find('input')
      .clear().focus().type(updated_name);

    cy.get('chef-button.save-button').click();
    cy.wait('@updateUser');
    cy.contains(updated_name).should('exist');

    cy.get('[formcontrolname=newPassword]').find('input')
      .focus().type(updated_password);
    cy.get('[formcontrolname=confirmPassword]').find('input')
      .focus().type(updated_password);
    cy.get('chef-button').contains('Update Password').click({ force: true} );

    // success alert displays
    cy.get('chef-notification.info').should('be.visible');

    // back to user list page
    cy.get('.breadcrumb').contains('Users').click();
  });

  it('can delete user', () => {
    cy.route('DELETE', '**/users/**').as('deleteUser');

    cy.get('chef-tbody chef-tr').contains(name).parent().parent()
      .find('chef-control-menu').as('controlMenu');

    cy.get('@controlMenu').click().then(($menu) => {
      $menu.find('[data-cy=delete]').click();
      // confirm in modal
      cy.get('chef-button').contains('Delete User').click();

      cy.wait('@deleteUser');
      cy.get('chef-tbody chef-td').contains(username).should('not.exist');
    });
  });
});
