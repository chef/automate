import { itFlaky } from '../../../support/constants';

describe('team add users', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'test-add-users';
  const nameForUser = cypressPrefix + ' user ' + now;
  const usernameForUser = 'testing-user-' + now;
  const descriptionForTeam = cypressPrefix + ' team ' + now;
  const nameForTeam = 'testing-team-' + now;
  let teamID = '';
  let adminIdToken = '';

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      // clean up leftover teams in case of previous test failures
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.cleanupUsersByNamePrefix(cypressPrefix);
      cy.cleanupTeamsByDescriptionPrefix(cypressPrefix);

      // create custom user and team
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/api/v0/auth/users',
        body: {
          username: usernameForUser,
          name: nameForUser,
          password: 'chefautomate'
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/teams',
        body: {
          description: descriptionForTeam,
          name: nameForTeam
        }
      }).then((resp) => {
        teamID = resp.body.team.id;
      });

    });
  });

  beforeEach(() => {
    cy.restoreStorage();

    cy.route('GET', `/apis/iam/v2/teams/${nameForTeam}`).as('getTeam');
    cy.route('GET', `/apis/iam/v2/teams/${nameForTeam}/users`).as('getTeamUsers');
    cy.route('GET', '/apis/iam/v2/users').as('getUsers');

    // TODO move this to the before block so it only happens once
    // since everytime we `visit` a new url instead of navigating via nav buttons
    // the whole app has to reload, slowing down the test and causing timeouts
    cy.visit(`/settings/teams/${nameForTeam}/add-users`);
    cy.wait(['@getTeam', '@getTeamUsers', '@getUsers']);
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupUsersByNamePrefix(cypressPrefix);
    cy.cleanupTeamsByDescriptionPrefix(cypressPrefix);
  });

  itFlaky('when the x is clicked, it returns to the team details page', () => {
    cy.get('chef-page chef-button.close-button').click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${nameForTeam}`);
  });

  itFlaky('when the cancel button is clicked, it returns to the team details page', () => {
    cy.get('#page-footer #right-buttons chef-button').last().click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${nameForTeam}`);
  });

  itFlaky('navigates to the team users add page', () => {
    cy.get('chef-page-header h1').contains(`Add Users to ${descriptionForTeam}`);

    cy.get('chef-table-body chef-table-row').contains('chef-table-row', usernameForUser)
      .contains(nameForUser);

    // Assert that there's more than two users: the one we created and admin,
    // that always exists.
    cy.get('chef-table-body').find('chef-table-row').its('length').should('be.gte', 2);

    cy.get('#page-footer #right-buttons chef-button ng-container').first().contains('Add User');
  });

  itFlaky('adds a single user', () => {
    cy.get('chef-table-body').contains('chef-table-row', usernameForUser)
      .find('chef-checkbox').click();
    cy.get('#users-selected').contains('1 user selected');

    cy.get('#page-footer #right-buttons chef-button ng-container')
      .first().contains('Add User').click();

    // drops you back on the team details page with user in the team users table
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${nameForTeam}`);
    cy.get('chef-table-body').children().should('have.length', 1);
    cy.get('chef-table-body chef-table-cell a').first().contains(nameForUser);

    // remove user from team
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: `/api/v0/auth/users/${usernameForUser}`
    }).then((resp) => {
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'PUT',
        url: `/apis/iam/v2/teams/${teamID}/users`,
        body: {
          user_ids: [resp.body.id]
        }
      });
    });
  });

  itFlaky('adds all users then sees empty message on attempting to add more users', () => {
    // Note: we add one user, and there always is an admin user. So,
    // we don't need to care for singular texts here ("Add 1 user" etc).
    cy.get('chef-table-body').find('chef-table-row').then(rows => {
      const userCount = Cypress.$(rows).length;
      cy.get('chef-table-body chef-checkbox').click({ multiple: true }); // check all checkboxes
      cy.get('#users-selected').contains(`${userCount} users selected`);

      cy.get('#page-footer #right-buttons chef-button')
        .contains(`Add ${userCount} Users`).click();
    });

    // drops you back on the team details page with user in the team users table
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${nameForTeam}`);
    cy.get('chef-table-body chef-table-cell a').contains(nameForUser);
    cy.get('chef-table-body chef-table-cell a').contains('Local Administrator');

    // navigate back to add users and see empty page and message
    cy.get('chef-toolbar chef-button').contains('Add User').click();
    cy.url().should('eq',
    `${Cypress.config().baseUrl}/settings/teams/${nameForTeam}/add-users`);
    cy.get('chef-table-new').should('not.exist');
    cy.get('#no-users-container p')
      .contains('There are no more local users to add; create some more!');
    cy.get('#no-users-container p a')
      .should('have.attr', 'target', '_blank')
      .should('have.attr', 'href', '/settings/users');
  });
});
