describe('team management', () => {
  let adminToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress-test';
  const teamName = `${cypressPrefix} team ${now}`;
  const teamID = `${cypressPrefix}-testing-team-custom-id-${now}`;
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;
  const unassigned = '(unassigned)';
  let teamUIRouteIdentifier = '';
  const nameForUser = cypressPrefix + ' user ' + now;
  const usernameForUser = cypressPrefix + 'testing-user-' + now;
  let userMembershipID = '';

  let iamVersion = <string><Object>Cypress.env('IAM_VERSION');
  // assume 2.0 if not in CI. if you wish something different start cypress with
  // IAM_VERSION set to what you are testing.
  if (iamVersion === undefined) {
    iamVersion = 'v2.0';
  }

  const describeIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
  const describeProjectsEnabled = iamVersion === 'v2.1' ? describe : describe.skip;

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;

      cy.cleanupUsersByNamePrefix(adminToken, cypressPrefix);
      cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
      cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/api/v0/auth/users',
        body: {
          username: usernameForUser,
          name: nameForUser,
          password: 'chefautomate'
        }
      }).then((resp) => {
        userMembershipID = resp.body.id;
      });

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/apis/iam/v2beta/projects',
        body: {
          id: project1ID,
          name: project1Name
        }
      });

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/apis/iam/v2beta/projects',
        body: {
          id: project2ID,
          name: project2Name
        }
      });

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/api/v0/auth/teams',
        body: {
          name: teamID,
          description: teamName
        }
      }).then((resp) => {
        const guid = resp.body.team.id;

        if (iamVersion.match(/v2/)) {
          teamUIRouteIdentifier = teamID;
        } else {
          teamUIRouteIdentifier = guid;
        }

        cy.request({
          auth: { bearer: adminToken },
          method: 'POST',
          url: `/apis/iam/v2beta/teams/${teamID}/users:add`,
          body: {
            user_ids: [userMembershipID]
          }
        });

        cy.visit(`/settings/teams/${teamUIRouteIdentifier}`);
        cy.get('app-welcome-modal').invoke('hide');
      });
    });

    cy.restoreStorage();
  });

  // it('displays team details for admins team', () => {
  //   cy.get('chef-breadcrumbs').contains('Teams');
  //   cy.get('chef-breadcrumbs').contains(teamName);

  //   cy.get('.page-title').contains(teamName);
  //   cy.contains('Add User');
  // });

  // context('when the team has users', () => {
  //   it('displays team users', () => {
  //     cy.get('chef-option').contains('Users');
  //     cy.get('app-user-table chef-th').contains('Name');
  //     cy.get('app-user-table chef-th').contains('Username');
  //     cy.get('app-user-table chef-td').contains(usernameForUser);
  //     cy.get('app-user-table chef-td').contains(nameForUser);
  //   });
  // });

  describeProjectsEnabled('update team projects (IAM v2.1 only)', () => {
    const dropdownNameUntilEllipsisLen = 25;

    context('when only the unassigned project is selected', () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned]);
      });

      it('cannot access projects dropdown but changing name allows team update submission', () => {
        cy.get('[data-cy=team-details-tab-details]').click();
        cy.get('[data-cy=team-details-name-input]').should('have.value', teamName);
        cy.get('[data-cy=team-details-submit-button]').should('have.attr', 'aria-disabled');

        // initial state of dropdown
        cy.get('app-team-details app-projects-dropdown #projects-selected').contains(unassigned);
        cy.get('app-projects-dropdown .dropdown-button').should('have.attr', 'disabled');

        cy.get('[data-cy=team-details-name-input]').type('updated name');
        cy.get('[data-cy=team-details-submit-button]').should('not.have.attr', 'aria-disabled');
      });
    });
  });
});
