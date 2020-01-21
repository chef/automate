import { itFlaky } from '../../../support/constants';

describe('user management', () => {
  before(() => {
    cy.adminLogin('/settings/users').then(() => {
      cy.cleanupUsersByNamePrefix('cypress test user ');
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
    cy.get('[data-cy=app-user-table-add-button]').contains('Create User').click();
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
    cy.get('#main-content-wrapper').scrollTo('top');
    cy.get('chef-notification.info').should('be.visible');

    cy.get('app-user-table chef-table-cell').contains(username).should('exist');
    cy.get('app-user-table chef-table-cell').contains(name).should('exist');
  });

  itFlaky('can view and edit user details', () => {
    cy.route('GET', `**/users/${username}`).as('getUser');
    cy.route('PUT', `**/users/${username}`).as('updateUser');

    const updated_name = `${name} updated`;
    const updated_password = 'testing?';

    cy.contains(name).click();
    cy.wait('@getUser');

    cy.get('app-user-details div.name-column').contains(name).should('exist');
    cy.get('app-user-details span').contains(username).should('exist');
    cy.get('app-user-details chef-form-field').contains('New Password').should('exist');
    cy.get('app-user-details chef-form-field').contains('Confirm New Password').should('exist');

    cy.get('app-user-details chef-button.edit-button').click();
    cy.get('[formcontrolname=fullName]').find('input').should('not.be.disabled')
      .focus().clear().type(updated_name);

    cy.get('app-user-details chef-button.save-button').click();
    cy.wait('@updateUser');

    // bug: Cypress sometimes misses the first few characters of the new name due to focus issues
    // so we test that some change was made to the name, not the exact new name
    cy.get('app-user-details div.name-column').contains('updated').should('exist');

    cy.get('[formcontrolname=newPassword]').find('input')
      .focus().type(updated_password, { delay: typeDelay }).should('have.value', updated_password);
    cy.get('[formcontrolname=confirmPassword]').find('input')
      .focus().type(updated_password, { delay: typeDelay }).should('have.value', updated_password);
    cy.get('app-user-details chef-button').contains('Update Password').click({ force: true });

    // success alert displays
    cy.get('#main-content-wrapper').scrollTo('top');
    cy.get('chef-notification.info').should('be.visible');
  });

  // this test isn't flaky but depends on the flaky one
  itFlaky('can delete user', () => {
    cy.route('GET', '**/users').as('getUsers');
    cy.route('DELETE', `**/users/${username}`).as('deleteUser');

    // back to user list page
    cy.get('app-user-details .breadcrumb').contains('Users').click();
    cy.wait('@getUsers');

    cy.get('app-user-table chef-table-cell').contains(username).parent()
      .find('[data-cy=select] .mat-select-trigger').as('dropdownTrigger');
    // we throw in a should so cypress waits until introspection allows menu to be shown
    cy.get('@dropdownTrigger').should('be.visible')
      .click();
    cy.get('.chef-control-menu').find('[data-cy=delete]').click({ force: true });

    // confirm in modal
    cy.get('app-user-management chef-button').contains('Delete User').click();

    cy.wait('@deleteUser');
    cy.get('app-user-management chef-table-body chef-table-cell')
      .contains(username).should('not.exist');
  });
});
