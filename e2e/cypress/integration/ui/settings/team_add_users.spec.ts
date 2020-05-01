describe('team add users', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'test-add-users';
  const userName = `${cypressPrefix} name ${now}`;
  const userID = `${cypressPrefix}-add-users-${now}`;
  const teamName = `${cypressPrefix} team ${now}`;
  const teamID = `${cypressPrefix}-team-${now}`;
  let adminIdToken = '';

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      // clean up leftover teams in case of previous test failures
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users', 'teams']);

      // create custom user and team
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/users',
        body: {
          id: userID,
          name: userName,
          password: 'chefautomate'
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/teams',
        body: {
          id: teamID,
          name: teamName
        }
      });
    });
  });

  beforeEach(() => {
    cy.restoreStorage();

    cy.route('GET', `/apis/iam/v2/teams/${teamID}`).as('getTeam');
    cy.route('GET', `/apis/iam/v2/teams/${teamID}/users`).as('getTeamUsers');
    cy.route('GET', '/apis/iam/v2/users').as('getUsers');

    // TODO move this to the before block so it only happens once
    // since everytime we `visit` a new url instead of navigating via nav buttons
    // the whole app has to reload, slowing down the test and causing timeouts
    cy.visit(`/settings/teams/${teamID}/add-users`);
    cy.wait(['@getTeam', '@getTeamUsers', '@getUsers']);
    cy.get('app-welcome-modal').invoke('hide');
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users', 'teams']);
  });

  it('when the x is clicked, it returns to the team details page', () => {
    cy.get('chef-page chef-button.close-button').click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamID}`);
  });

  it('when the cancel button is clicked, it returns to the team details page', () => {
    cy.get('#page-footer #right-buttons chef-button').last().click();
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamID}`);
  });

  it('navigates to the team users add page', () => {
    cy.get('chef-page-header h1').contains(`Add Users to ${teamName}`);

    cy.get('chef-tbody chef-tr').contains(userID);

    // Assert that there's more than two users: the one we created and admin,
    // that always exists.
    cy.get('chef-tbody').find('chef-tr').its('length').should('be.gte', 2);

    cy.get('#page-footer #right-buttons chef-button ng-container').first().contains('Add User');
  });

  it('adds a single user', () => {
    cy.get('chef-tbody').contains('chef-tr', userID)
      .find('chef-checkbox').click();
    cy.get('#users-selected').contains('1 user selected');

    cy.get('#page-footer #right-buttons chef-button ng-container')
      .first().contains('Add User').click();

    // drops you back on the team details page with user in the team users table
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamID}`);
    cy.get('chef-tbody').children().should('have.length', 1);
    cy.get('chef-tbody chef-td a').first().contains(userName);

    // remove user from team
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: `/apis/iam/v2/users/${userID}`
    }).then((resp) => {
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: `/apis/iam/v2/teams/${teamID}/users:remove`,
        body: {
          membership_ids: [resp.body.user.membership_id]
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
    cy.url().should('eq', `${Cypress.config().baseUrl}/settings/teams/${teamID}`);
    cy.get('chef-tbody chef-td a').contains(userName);
    cy.get('chef-tbody chef-td a').contains('Local Administrator');

    // navigate back to add users and see empty page and message
    cy.get('chef-toolbar chef-button').contains('Add Users').click();
    cy.url().should('eq',
    `${Cypress.config().baseUrl}/settings/teams/${teamID}/add-users`);
    cy.get('chef-table').should('not.exist');
    cy.get('#no-users-container p')
      .contains('All local users have already been added; create some more!');
    cy.get('chef-button').contains('Create User');
  });

  // TODO test creating a user on the Add Users modal
});
