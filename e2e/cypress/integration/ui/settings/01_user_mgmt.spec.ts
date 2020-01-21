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
    cy.get('[formcontrolname=displayName]')
    .type(' update', { delay: typeDelay }).should('have.value', name + ' update');

    // save display name change
    cy.get('[data-cy=user-details-submit-button]').click();
    cy.get('app-user-details span#saved-note').contains('All changes saved.').should('exist');
    cy.get('app-user-details div.name-column').contains(name + ' update').should('exist');
  });
});
