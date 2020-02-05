import { itFlaky } from '../../../support/constants';

describe('team details', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'test-team-details';
  const teamName = `${cypressPrefix} team ${now}`;
  const teamID = `${cypressPrefix}-testing-team-custom-id-${now}`;
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;
  const unassigned = '(unassigned)';
  const nameForUser = cypressPrefix + ' user ' + now;
  const usernameForUser = cypressPrefix + 'testing-user-' + now;
  let userMembershipID = '';

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;

      cy.cleanupUsersByNamePrefix(cypressPrefix);
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
      cy.cleanupTeamsByDescriptionPrefix(cypressPrefix);

      cy.request({
        auth: { bearer: adminIdToken },
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
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project1ID,
          name: project1Name
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project2ID,
          name: project2Name
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2/teams',
        body: {
          name: teamID,
          description: teamName
        }
      }).then((resp) => {
        cy.request({
          auth: { bearer: adminIdToken },
          method: 'POST',
          url: `/apis/iam/v2/teams/${teamID}/users:add`,
          body: {
            user_ids: [userMembershipID]
          }
        });

        cy.visit(`/settings/teams/${teamID}#users`);
        cy.get('app-welcome-modal').invoke('hide');
      });
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
  });

  it('displays team details for admins team', () => {
    cy.get('chef-breadcrumbs').contains('Teams');
    cy.get('chef-breadcrumbs').contains(teamName);

    cy.get('.page-title').contains(teamName);
    cy.contains('Add User');
  });

  context('when the team has users', () => {
    it('displays team users', () => {
      cy.get('chef-option').contains('Users');
      cy.get('app-user-table chef-table-header-cell').contains('Name');
      cy.get('app-user-table chef-table-header-cell').contains('Username');
      cy.get('app-user-table chef-table-cell').contains(usernameForUser);
      cy.get('app-user-table chef-table-cell').contains(nameForUser);
    });
  });

  describe('update team projects', () => {
    const dropdownNameUntilEllipsisLen = 25;

    context('when only the unassigned project is selected', () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned]);
      });

      it('cannot access projects dropdown but can still update name', () => {
        cy.get('[data-cy=team-details-tab-details]').click();

        // initial state of page
        cy.get('[data-cy=team-details-submit-button]').should('have.attr', 'aria-disabled');
        cy.get('app-team-details app-projects-dropdown #projects-selected').contains(unassigned);
        cy.get('app-team-details app-projects-dropdown .dropdown-button').should('be.disabled');

        cy.get('[data-cy=team-details-name-input]')
          .should('have.value', teamName).should('not.be.disabled').type('updated name');
        cy.get('[data-cy=team-details-submit-button]').should('not.have.attr', 'aria-disabled');
      });
    });

    context('when the team contains a project', () => {
      beforeEach(() => {
        cy.request({
          auth: { bearer: adminIdToken },
          method: 'PUT',
          url: `/apis/iam/v2/teams/${teamID}`,
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
          auth: { bearer: adminIdToken },
          method: 'PUT',
          url: `/apis/iam/v2/teams/${teamID}`,
          body: {
            name: teamName,
            projects: []
          }
        });
      });

      context('when the project filter contains team project and other project', () => {
        beforeEach(() => {
          // TODO (tc): Note that as stands, if you ever update a team to only contain projects
          // not in the project filter -- including (unassigned) -- you'll get an error on save
          // since the project filter is applied to the request to re-fetch the team. Known issue
          // we are going to address in future work.
          cy.applyProjectsFilter([unassigned, project1Name, project2Name]);
        });

        itFlaky('both are contained in the projects dropdown and the team project is selected,' +
              'and both can be added or removed', () => {
          const projectSummary = '2 projects';

          cy.get('[data-cy=team-details-tab-details]').click();
          cy.get('[data-cy=team-details-name-input]').should('have.value', teamName);
          cy.get('[data-cy=team-details-submit-button]').should('have.attr', 'aria-disabled');

          // initial state of dropdown
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(`${project1Name.substring(0, dropdownNameUntilEllipsisLen)}...`);
          cy.get('app-team-details app-projects-dropdown .dropdown-button')
            .should('not.be.disabled');

          // open projects dropdown
          cy.get('app-team-details app-projects-dropdown .dropdown-button').click();

          // dropdown contains both custom projects, one selected already, click the other
          cy.get(`app-team-details app-projects-dropdown chef-checkbox[title="${project1Name}"]`)
            .should('have.attr', 'aria-checked', 'true');
          cy.get(`app-team-details app-projects-dropdown chef-checkbox[title="${project2Name}"]`)
            .should('have.attr', 'aria-checked', 'false').find('chef-icon').click();
          cy.get('app-team-details app-projects-dropdown .dropdown-button').click();

          // save
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(projectSummary);
          cy.get('[data-cy=team-details-submit-button]').click();

          // de-select project1 and project2
          cy.get('app-team-details app-projects-dropdown .dropdown-button')
            .should('not.be.disabled').click();
          cy.get(`app-team-details app-projects-dropdown chef-checkbox[title="${project1Name}"]`)
            .should('have.attr', 'aria-checked', 'true').find('chef-icon').click();
          cy.get(`app-team-details app-projects-dropdown chef-checkbox[title="${project2Name}"]`)
            .should('have.attr', 'aria-checked', 'true').find('chef-icon').click();
          cy.get('app-team-details app-projects-dropdown .dropdown-button').click();

          // save
          cy.get('app-team-details app-projects-dropdown #projects-selected')
            .contains(unassigned);
          cy.get('[data-cy=team-details-submit-button]').click();
        });
      });
    });
  });
});
