describe('team add users', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress test';
  const nameForUser = cypressPrefix + ' user ' + now;
  const usernameForUser = 'testing-user-' + now;
  const descriptionForTeam = cypressPrefix + ' team ' + now;
  const nameForTeam = 'testing-team-' + now;
  let teamID = '';
  let adminToken = '';
  let teamUIRouteIdentifier = '';
  let iamVersion = '';

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      // clean up leftover teams in case of previous test failures
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;
      cy.cleanupUsersByNamePrefix(adminToken, cypressPrefix);
      cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);

      // create custom user and team
      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/api/v0/auth/users',
        body: {
          username: usernameForUser,
          name: nameForUser,
          password: 'chefautomate'
        }
      });

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/api/v0/auth/teams',
        body: {
          description: descriptionForTeam,
          name: nameForTeam
        }
      }).then((resp) => {
        teamID = resp.body.team.id;

        cy.get('chef-sidebar')
        .invoke('attr', 'major-version')
        .then((obj: Cypress.ObjectLike) => {
          // Cypress.ObjectLike can't be casted to a string directly,
          // so must convert to Object type (common to all JS objects) first
          iamVersion = <string><Object>obj;

          if (iamVersion === 'v2') {
            teamUIRouteIdentifier = nameForTeam;
          } else {
            teamUIRouteIdentifier = teamID;
          }
        });
      });

    });
  });

  beforeEach(() => {
    cy.restoreStorage();

    if (iamVersion === 'v2') {
      cy.route('GET', `/apis/iam/v2beta/teams/${nameForTeam}`).as('getTeam');
      cy.route('GET', `/apis/iam/v2beta/teams/${nameForTeam}/users`).as('getTeamUsers');
      cy.route('GET', '/apis/iam/v2beta/users').as('getUsers');
    } else {
      cy.route('GET', `/api/v0/auth/teams/${teamID}`).as('getTeam');
      cy.route('GET', `/api/v0/auth/teams/${teamID}/users`).as('getTeamUsers');
      cy.route('GET', '/api/v0/auth/users').as('getUsers');
    }

    cy.visit(`/settings/teams/${teamUIRouteIdentifier}/add-users`);
    cy.wait(['@getTeam', '@getTeamUsers', '@getUsers']);
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupUsersByNamePrefix(adminToken, cypressPrefix);
    cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
  });

  it('when the x is clicked, it returns to the team details page', () => {
    cy.get('chef-page chef-button.close-button').click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamUIRouteIdentifier}`);
  });

  it('when the cancel button is clicked, it returns to the team details page', () => {
    cy.get('#page-footer #right-buttons chef-button').last().click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamUIRouteIdentifier}`);
  });

  it('navigates to the team users add page', () => {
    cy.get('chef-page-header h1').contains(`Add Users to ${descriptionForTeam}`);

    cy.get('chef-tbody chef-tr').contains('chef-tr', usernameForUser)
      .contains(nameForUser);

    // The admin user exists by default
    cy.get('chef-tbody chef-tr').contains('chef-tr', 'admin')
      .contains('Local Administrator');

    cy.get('#page-footer #right-buttons chef-button ng-container').first().contains('Add User');
  });

  it('adds a single user', () => {
    cy.get('chef-tbody chef-checkbox').first().click();
    cy.get('#users-selected').contains('1 user selected');

    cy.get('#page-footer #right-buttons chef-button ng-container')
      .first().contains('Add User').click();

    // drops you back on the team details page with user in the team users table
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamUIRouteIdentifier}`);
    cy.get('chef-tbody').children().should('have.length', 1);
    cy.get('chef-tbody chef-td a').first().contains(nameForUser);

    // remove user from team
    cy.request({
      auth: { bearer: adminToken },
      method: 'GET',
      url: `/api/v0/auth/users/${usernameForUser}`
    }).then((resp) => {
      cy.request({
        auth: { bearer: adminToken },
        method: 'PUT',
        url: `/api/v0/auth/teams/${teamID}/users`,
        body: {
          user_ids: [resp.body.id]
        }
      });
    });
  });

  it('adds all users then sees empty message on attempting to add more users', () => {
    // Note: we add one user, and there always is an admin user. So,
    // we don't need to care for singular texts here ("Add 1 user" etc).
    cy.get('chef-tbody').find('chef-tr').then(rows => {
      const userCount = Cypress.$(rows).length;
      cy.get('chef-tbody chef-checkbox').click({ multiple: true }); // check all checkboxes
      cy.get('#users-selected').contains(`${userCount} users selected`);

      cy.get('#page-footer #right-buttons chef-button')
        .contains(`Add ${userCount} Users`).click();
    });

    // drops you back on the team details page with user in the team users table
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamUIRouteIdentifier}`);
    cy.get('chef-tbody chef-td a').contains(nameForUser);
    cy.get('chef-tbody chef-td a').contains('Local Administrator');

    // navigate back to add users and see empty page and message
    cy.get('chef-toolbar chef-button').contains('Add Users').click();
    cy.url().should('eq',
    `${Cypress.config().baseUrl}/settings/teams/${teamUIRouteIdentifier}/add-users`);
    cy.get('chef-table').should('not.exist');
    cy.get('#no-users-container p')
      .contains('There are no more local users to add; create some more!');
    cy.get('#no-users-container p a')
      .should('have.attr', 'target', '_blank')
      .should('have.attr', 'href', '/settings/users');
  });
});
