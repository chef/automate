describe('team management', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'test-team-mgmt';
  const teamName = `${cypressPrefix} team ${now}`;
  const customTeamID = `${cypressPrefix}-custom-id-${now}`;
  const generatedTeamID = teamName.split(' ').join('-');
  const unassignedTeam1ID = `${cypressPrefix}-unassigned-1-${now}`;
  const unassignedTeam2ID = `${cypressPrefix}-unassigned-2-${now}`;
  const teamIDWithSomeProjects = `${cypressPrefix}-some-projects-${now}`;
  const teamIDWithOneProject = `${cypressPrefix}-one-project-${now}`;

  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;
  const unassigned = '(unassigned)';

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));

      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project1ID,
          name: project1Name,
          skip_policies: true
        }
      });
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: {
          id: project2ID,
          name: project2Name,
          skip_policies: true
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
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'teams']);
  });

  describe('teams list page', () => {
    const dropdownNameUntilEllipsisLen = 25;
    const projectDropdownPrefix = 'app-team-management app-projects-dropdown';

    it('lists system teams', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team');
      cy.get('#table-container chef-th').contains('ID');
      cy.get('#table-container chef-th').contains('Name');

      const systemTeams = ['admins', 'editors', 'viewers'];
      systemTeams.forEach(name => {
        cy.get('#table-container chef-tr').contains(name)
          .parent().parent().find('mat-select').as('control-menu');
      });
    });

    it('can create a team with a default ID', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=id-label]').contains(generatedTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.contains(teamName).should('exist');
      cy.contains(generatedTeamID).should('exist');
    });

    it('can create a team with a custom ID', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=create-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=create-id]').should('be.visible').clear().type(customTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal').should('not.be.visible');

      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.contains(teamName).should('exist');
      cy.contains(customTeamID).should('exist');
    });

    it('fails to create a team with a duplicate ID', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=id-label]').contains(generatedTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal chef-error').contains('already exists')
        .should('be.visible');

      // here we exit with the chef-modal exit button in the top right corner
      cy.get('app-team-management chef-modal chef-button.close').first().click();
    });

    it('can cancel creating a team', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('chef-button').contains('Cancel').should('be.visible').click();

      // here we exit with the Cancel button
      cy.get('app-team-management chef-modal').should('not.be.visible');
    });

    it('can delete a team', () => {
      cy.get('app-team-management chef-td').contains(customTeamID).parent()
        .find('.mat-select-trigger').as('controlMenu');

      // we throw in a `should` so cypress retries until introspection allows menu to be shown
      cy.get('@controlMenu').scrollIntoView().should('be.visible')
        .click();
      cy.get('[data-cy=delete-team]').should('be.visible')
        .click();

      // accept dialog
      cy.get('app-team-management chef-button').contains('Delete Team').click();

      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains('Deleted team');
      cy.get('app-notification.info chef-icon').click();

      cy.get('app-team-management chef-tbody chef-td')
        .contains(customTeamID).should('not.exist');
    });

    context('when only the unassigned project is selected', () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned]);
      });

      it(`can create a team with no projects (unassigned)
        and cannot access projects dropdown`, () => {
        if (Cypress.$('app-welcome-modal').length) {  // zero length means not found
          cy.get('[data-cy=close-x]').click();
        }
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(unassignedTeam1ID);
        cy.get('[data-cy=id-label]').contains(unassignedTeam1ID);

        // initial state of dropdown
        cy.get(`${projectDropdownPrefix} #resources-selected`)
          .contains(unassigned);
        cy.get(`${projectDropdownPrefix} .dropdown-button`)
          .should('have.attr', 'disabled');

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');

        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();

        cy.contains(unassignedTeam1ID).should('exist');
        cy.contains(unassigned).should('exist');
      });
    });

    context(`when there are multiple custom projects selected in the
      filter (including the unassigned project)`, () => {
      beforeEach(() => {
        cy.applyProjectsFilter([unassigned, project1Name, project2Name]);
        if (Cypress.$('app-welcome-modal').length) {  // zero length means not found
          cy.get('[data-cy=close-x]').click();
        }
      });

      it('can create a team with multiple projects', () => {
        const projectSummary = '2 projects';
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamIDWithSomeProjects);
        cy.get('[data-cy=id-label]').contains(teamIDWithSomeProjects);

        // initial state of dropdown
        cy.get(`${projectDropdownPrefix} #resources-selected`).contains(unassigned);
        cy.get(`${projectDropdownPrefix} .dropdown-button`).should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(`${projectDropdownPrefix} .dropdown-button`).click();

        // dropdown contains both custom projects, click them both
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();

        // close projects dropdown
        cy.get(`${projectDropdownPrefix} .dropdown-button`).click();
        cy.get(`${projectDropdownPrefix} #resources-selected`)
          .contains(projectSummary);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');

        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();

        cy.contains(teamIDWithSomeProjects).should('exist');
        cy.contains(projectSummary).should('exist');
      });

      it('can create a team with one project selected', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamIDWithOneProject);
        cy.get('[data-cy=id-label]').contains(teamIDWithOneProject);

        // initial state of dropdown
        cy.get(`${projectDropdownPrefix} #resources-selected`).contains(unassigned);
        cy.get(`${projectDropdownPrefix} .dropdown-button`).should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(`${projectDropdownPrefix} .dropdown-button`).click();

        // dropdown contains both custom projects, click one
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false');
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false').click();

        // close projects dropdown
        cy.get(`${projectDropdownPrefix} .dropdown-button`).click();
        cy.get(`${projectDropdownPrefix} #resources-selected`)
          .contains(`${project2Name.substring(0, dropdownNameUntilEllipsisLen)}...`);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');

        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();

        // TODO: find row
        cy.contains(teamIDWithOneProject).should('exist');
        cy.contains(project2ID).should('exist');
      });

      it('can create a team with no projects selected (unassigned)', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(unassignedTeam2ID);
        cy.get('[data-cy=id-label]').contains(unassignedTeam2ID);

        // initial state of dropdown
        cy.get(`${projectDropdownPrefix} #resources-selected`).contains(unassigned);
        cy.get(`${projectDropdownPrefix} .dropdown-button`).should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get(`${projectDropdownPrefix} .dropdown-button`).click();

        // dropdown contains both custom projects, none clicked
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project1Name}"]`)
          .should('have.attr', 'aria-checked', 'false');
        cy.get(`${projectDropdownPrefix} chef-checkbox[title="${project2Name}"]`)
          .should('have.attr', 'aria-checked', 'false');

        // close projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();
        cy.get('app-projects-dropdown #resources-selected').contains(unassigned);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management app-team-management chef-modal').should('not.be.visible');
        cy.get('#main-content-wrapper').scrollTo('top');

        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();

        // TODO: find row
        cy.contains(unassignedTeam2ID).should('exist');
        cy.contains(unassigned).should('exist');
      });
    });
  });
});
