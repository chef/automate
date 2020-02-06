import { itFlaky } from '../../../support/constants';

describe('team management', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'test-team-mgmt';
  const teamName = `${cypressPrefix} team ${now}`;
  const customTeamID = `${cypressPrefix}-custom-id-${now}`;
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;
  const generatedTeamID = teamName.split(' ').join('-');
  const unassigned = '(unassigned)';

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));

      // extra precaution in case the `after` cleanup didn't run due to a failure
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['teams', 'projects', 'policies']);

      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project1ID,
          name: project1Name
        }
      });
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project2ID,
          name: project2Name
        }
      });

      // reload so we get projects in project filter
      cy.reload(true);

      cy.get('app-welcome-modal').invoke('hide');
    });
    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
    // clean up teams in between each test so we can reuse teamName and teamID
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['teams']);
  });

  after(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
  });

  describe('no custom initial page state', () => {
    it('lists system teams', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team');
      cy.get('#table-container chef-table-header-cell').contains('ID');
      cy.get('#table-container chef-table-header-cell').contains('Name');

      const systemTeams = ['admins', 'editors', 'viewers'];
      systemTeams.forEach(name => {
        cy.get('#table-container chef-table-row').contains(name)
          .parent().parent().find('mat-select').as('control-menu');
      });
    });
  });

  describe('teams list page', () => {
    const dropdownNameUntilEllipsisLen = 25;
    const projectDropdown = 'app-team-management app-projects-dropdown ';

    itFlaky('can create a team with a default ID', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=id-label]').contains(generatedTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal').should('not.be.visible');
      cy.get('chef-notification.info').should('be.visible');
      cy.contains(teamName).should('exist');
      cy.contains(generatedTeamID).should('exist');
    });

    itFlaky('can create a team with a custom ID', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=create-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=create-id]').should('be.visible').clear().type(customTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal').should('not.be.visible');
      cy.get('#main-content-wrapper').scrollTo('top');
      cy.get('chef-notification.info').should('be.visible');
      cy.contains(teamName).should('exist');
      cy.contains(customTeamID).should('exist');
    });


    context('when only the unassigned project is selected', () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned]);
      });

      itFlaky(`can create a team with no projects (unassigned)
        and cannot access projects dropdown`, () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get(projectDropdown + '#projects-selected')
          .contains(unassigned);
        cy.get(projectDropdown + '.dropdown-button')
          .should('have.attr', 'disabled');

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.contains(unassigned).should('exist');
      });
    });

    context(`when there are multiple custom projects selected in the
      filter (including the unassigned project)`, () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned, project1Name, project2Name]);
      });

      after(() => {
        cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
      });

      afterEach(() => {
        // clean up teams in between each test so we can reuse teamName and teamID
        cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['teams']);
      });

      itFlaky('can create a team with multiple projects', () => {
        const projectSummary = '2 projects';
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get(projectDropdown + '#projects-selected').contains(unassigned);
        cy.get(projectDropdown + '.dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(projectDropdown + '.dropdown-button').click();

        // dropdown contains both custom projects, click them both
        cy.get(projectDropdown + `chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();
        cy.get(projectDropdown + `chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();

        // close projects dropdown
        cy.get(projectDropdown + '.dropdown-button').click();
        cy.get(projectDropdown + '#projects-selected')
          .contains(projectSummary);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.contains(projectSummary).should('exist');
      });

      itFlaky('can create a team with one project selected', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get(projectDropdown + '#projects-selected').contains(unassigned);
        cy.get(projectDropdown + '.dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(projectDropdown + '.dropdown-button').click();

        // dropdown contains both custom projects, click one
        cy.get(projectDropdown + `chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false');
        cy.get(projectDropdown + `chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();

        // close projects dropdown
        cy.get(projectDropdown + '.dropdown-button').click();
        cy.get(projectDropdown + '#projects-selected')
          .contains(`${project2Name.substring(0, dropdownNameUntilEllipsisLen)}...`);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.contains(project2ID).should('exist');
      });

      itFlaky('can create a team with no projects selected (unassigned)', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get(projectDropdown + '#projects-selected').contains(unassigned);
        cy.get(projectDropdown + '.dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(projectDropdown + '.dropdown-button').click();

        // dropdown contains both custom projects, none clicked
        cy.get(projectDropdown + `chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false');
        cy.get(projectDropdown + `chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false');

        // close projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.contains(unassigned).should('exist');
      });
    });
  });
});
