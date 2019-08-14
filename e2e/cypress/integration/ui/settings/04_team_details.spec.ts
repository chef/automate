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

      // it('cannot access projects dropdown but changing name allows team update submission', () => {
      //   cy.get('[data-cy=team-details-tab-details]').click();
      //   cy.get('[data-cy=team-details-name-input]').should('have.value', teamName);
      //   cy.get('[data-cy=team-details-submit-button]').should('have.attr', 'aria-disabled');

      //   // initial state of dropdown
      //   cy.get('app-team-details app-projects-dropdown #projects-selected').contains(unassigned);
      //   cy.get('app-projects-dropdown .dropdown-button').should('have.attr', 'disabled');

      //   cy.get('[data-cy=team-details-name-input]').type('updated name');
      //   cy.get('[data-cy=team-details-submit-button]').should('not.have.attr', 'aria-disabled');
      // });
    });

    context('when the team contains a project', () => {
      beforeEach(() => {
        cy.request({
          auth: { bearer: adminToken },
          method: 'PUT',
          url: `/apis/iam/v2beta/teams/${teamUIRouteIdentifier}`,
          body: {
            name: teamName,
            projects: [project1ID]
          }
        });
        cy.reload(true);
        cy.get('app-welcome-modal').invoke('hide');
      });

      afterEach(() => {
        cy.request({
          auth: { bearer: adminToken },
          method: 'PUT',
          url: `/apis/iam/v2beta/teams/${teamUIRouteIdentifier}`,
          body: {
            name: teamName,
            projects: []
          }
        });
      });

      context('when the project filter contains team project and other project', () => {
        beforeEach(() => {
          cy.route('GET', `/apis/iam/v2beta/projects/${project1ID}`).as('getProjectForTeam');
          // TODO (tc): Note that as stands, if you ever update a team to only contain projects
          // not in the project filter -- including (unassigned) -- you'll get an error on save
          // since the project filter is applied to the request to re-fetch the team. Known issue
          // we are going to address in future work.
          cy.applyProjectsFilter([unassigned, project1Name, project2Name]);

          // Wait for the project contained in the team to 
          cy.wait('@getProjectForTeam');
        });

        it('both are contained in the projects dropdown and the team project is selected,' +
              'and both can be added or removed', () => {
          const projectSummary = '2 projects';

          cy.get('[data-cy=team-details-tab-details]').click();
          cy.get('[data-cy=team-details-name-input]').should('have.value', teamName);
          cy.get('[data-cy=team-details-submit-button]').should('have.attr', 'aria-disabled');

          // initial state of dropdown
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(`${project1Name.substring(0, dropdownNameUntilEllipsisLen)}...`);
          cy.get('app-team-details app-projects-dropdown .dropdown-button')
            .should('not.have.attr', 'disabled');

          // open projects dropdown
          cy.get('app-team-details app-projects-dropdown .dropdown-button').click();

          // dropdown contains both custom projects, one selected already, click the other
          cy.get('app-projects-dropdown chef-dropdown')
            .children('chef-checkbox').contains(project1Name)
            .should('have.attr', 'aria-checked', 'true');
          cy.get('app-projects-dropdown chef-dropdown')
            .children('chef-checkbox').contains(project2Name)
            .should('not.have.attr', 'aria-checked', 'true').click();

          // save
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(projectSummary);
          cy.get('[data-cy=team-details-submit-button]')
            .should('not.have.attr', 'aria-disabled').click();

          // de-select project1 and project2
          cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project1Name)
            .should('have.attr', 'aria-checked', 'true').click();
          cy.get('app-projects-dropdown chef-dropdown')
            .children('chef-checkbox').contains(project2Name)
            .should('have.attr', 'aria-checked', 'true').click();

          // save
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(unassigned);
          cy.get('[data-cy=team-details-submit-button]')
            .should('not.have.attr', 'aria-disabled').click();
        });
      });
    });
  });
});
