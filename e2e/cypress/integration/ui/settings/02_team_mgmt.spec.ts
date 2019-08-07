describe('team management', () => {
  let adminToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress-test';
  const teamName = `${cypressPrefix} team ${now}`;
  const customTeamID = `${cypressPrefix}-testing-team-custom-id-${now}`;
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;
  const generatedTeamID = teamName.split(' ').join('-');
  const unassigned = '(unassigned)';

  let iamVersion = <string><Object>Cypress.env('IAM_VERSION');
  // assume 2.0 if not in CI. if you wish something different start cypress with
  // IAM_VERSION set to what you are testing.
  if (iamVersion === undefined) {
    iamVersion = 'v2.0';
  }

  const describeIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
  const describeProjectsEnabled = iamVersion === 'v2.1' ? describe : describe.skip;

  afterEach(() => {
    cy.saveStorage();
    cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
  });

  context('no custom initial page state', () => {
    beforeEach(() => {
      cy.adminLogin('/settings/teams').then(() => {
        const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
        adminToken = admin.id_token;
        cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
        cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
      });
      cy.restoreStorage();
    });

    it('lists system teams', () => {
      cy.get('[data-cy=team-create-button]').contains('Create Team');
      cy.get('chef-sidebar')
        .invoke('attr', 'major-version')
        .then((obj: Cypress.ObjectLike) => {
          switch (<string><Object>obj) {
            case 'v2': {
              cy.get('#table-container chef-th').contains('ID');
              cy.get('#table-container chef-th').contains('Name');

              const systemTeams = ['admins', 'editors', 'viewers'];
              systemTeams.forEach(name => {
              cy.get('#table-container chef-tr').contains(name)
                .parent().parent().find('chef-control-menu').as('control-menu');
              });
              break;
            }
            default: {
              cy.get('#table-container chef-th').contains('Name');
              cy.get('#table-container chef-th').contains('Description');

              const systemTeams = ['admins'];
              systemTeams.forEach(name => {
              cy.get('#table-container chef-tr').contains(name)
                .parent().parent().find('chef-control-menu').as('control-menu');
              });
            }
          }
      });
    });

    it('displays team details for admins team', () => {
      cy.get('chef-td').contains('admins').click();

      cy.get('chef-breadcrumbs').contains('Teams');
      cy.get('chef-breadcrumbs').contains('admins');

      cy.get('.page-title').contains('admins');
      cy.contains('Add User');
    });

    it('displays team users for admins team', () => {
      cy.get('chef-td').contains('admins').click();
      cy.get('chef-option').contains('Users');
      cy.get('app-user-table chef-th').contains('Name');
      cy.get('app-user-table chef-th').contains('Username');
      cy.get('app-user-table chef-td').contains('Local Administrator');
      cy.get('app-user-table chef-td').contains('admin');
    });

    describeIAMV2('team create modal (IAM v2.x)', () => {
      it('can create a team with a default ID', () => {
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
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(customTeamID).should('exist');
        cy.contains(generatedTeamID).should('not.exist');
      });
    });
  });

  describeProjectsEnabled('team create modal with projects (IAM v2.1 only)', () => {
    const unusedProject = 'cypress-unused';
    const STORE_OPTIONS_KEY = 'projectsFilter.options';
    const dropdownNameUntilEllipsisLen = 25;

    context('when only the unassigned project is selected', () => {
      beforeEach(() => {
        localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify([
          {label: unassigned, value: unassigned, type: 'CUSTOM', checked: true},
          {label: project1Name, value: project1ID, type: 'CUSTOM', checked: false},
          {label: project2Name, value: project2ID, type: 'CUSTOM', checked: false},
          {label: unusedProject, value: unusedProject, type: 'CUSTOM', checked: false}
        ]));
        cy.adminLogin('/settings/teams').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminToken = admin.id_token;
          cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
          cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
        });
        cy.restoreStorage();
        // Necessary to pick up localstorage changes
        cy.reload(true);
        cy.get('app-welcome-modal').invoke('hide');
      });

      it('can create a team with no projects (unassigned) ' +
      'and cannot access projects dropdown', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);

        // TODO (tc) why doesn't this work?
        // cy.get('app-projects-dropdown .dropdown-button').should('have.attr', 'disabled', true);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.get('[data-cy=team-details-projects]').contains(unassigned);
      });
    });

    context('when there are multiple custom projects selected in the ' +
      'filter (including the unassinged project)', () => {
      beforeEach(() => {
        localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify([
          {label: unassigned, value: unassigned, type: 'CUSTOM', checked: true},
          {label: project1Name, value: project1ID, type: 'CUSTOM', checked: true},
          {label: project2Name, value: project2ID, type: 'CUSTOM', checked: true},
          {label: unusedProject, value: unusedProject, type: 'CUSTOM', checked: false}
        ]));
        cy.adminLogin('/settings/teams').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminToken = admin.id_token;
          cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
          cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
        });
        cy.restoreStorage();
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
        // Necessary to pick up localstorage changes
        cy.reload(true);
        cy.get('app-welcome-modal').invoke('hide');
      });

      after(() => {
        cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
      });

      afterEach(() => {
        cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
      });

      it('can create a team with mulitple projects', () => {
        const projectSummary = '2 projects';
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);
        cy.get('app-projects-dropdown .dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();

        // dropdown contains both custom projects, click them both
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project1Name).click();
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project2Name).click();

        // close projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();
        cy.get('app-projects-dropdown #projects-selected').contains(projectSummary);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.get('[data-cy=team-details-projects]').contains(projectSummary);
      });

      it('can create a team with one project selected', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);
        cy.get('app-projects-dropdown .dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();

        // dropdown contains both custom projects, click one
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project1Name);
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project2Name).click();

        // close projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();
        cy.get('app-projects-dropdown #projects-selected')
          .contains(`${project2Name.substring(0, dropdownNameUntilEllipsisLen)}...`);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.get('[data-cy=team-details-projects]').contains(project2ID);
      });

      it('can create a team with no projects selected (unassigned)', () => {
        cy.get('[data-cy=team-create-button]').contains('Create Team').click();
        cy.get('app-team-management chef-modal').should('exist');
        cy.get('[data-cy=create-name]').type(teamName);
        cy.get('[data-cy=id-label]').contains(generatedTeamID);

        // initial state of dropdown
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);
        cy.get('app-projects-dropdown .dropdown-button').should('not.have.attr', 'disabled');

        // open projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();

        // dropdown contains both custom projects, none clicked
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project1Name);
        cy.get('app-projects-dropdown chef-dropdown')
          .children('chef-checkbox').contains(project2Name);

        // close projects dropdown
        cy.get('app-projects-dropdown .dropdown-button').click();
        cy.get('app-projects-dropdown #projects-selected').contains(unassigned);

        // save team
        cy.get('[data-cy=save-button]').click();
        cy.get('app-team-management chef-modal').should('not.be.visible');
        cy.get('chef-notification.info').should('be.visible');
        cy.contains(teamName).should('exist');
        cy.contains(generatedTeamID).should('exist');
        cy.get('[data-cy=team-details-projects]').contains(unassigned);
      });
    });
  });
});
