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
  // assume 2.1 if not in CI. if you wish something different start cypress with
  // IAM_VERSION set to what you are testing.
  if (iamVersion === undefined) {
    iamVersion = 'v2.1';
  }

  const describeIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
  const describeProjectsEnabled = iamVersion === 'v2.1' ? describe : describe.skip;

  before(() => {
    cy.adminLogin('/settings/teams').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;
      cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
      cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
    cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
  });

  it('lists system teams', () => {
    cy.contains('Create Team');
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

  describeIAMV2('team create modal (IAM v2.x)', () => {
    it('can create a team with a default ID', () => {
      cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
      cy.get('app-team-management chef-modal').should('exist');

      cy.get('[data-cy=create-name]').type(teamName);

      cy.get('[data-cy=id-label]').contains(generatedTeamID);

      cy.get('[data-cy=save-button]').click();
      cy.get('app-team-management chef-modal').should('not.be.visible');
      cy.get('chef-notification.info').should('be.visible');
      cy.contains(teamName).should('exist');
      cy.contains(generatedTeamID).should('exist');
      cy.go('back');
    });

    it('can create a team with a custom ID', () => {
      cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
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
      cy.go('back');
    });
  });

  describeProjectsEnabled('team create modal with projects (IAM v2.1 only)', () => {
    const unusedProject = 'cypress-unused';
    const STORE_OPTIONS_KEY = 'projectsFilter.options';
    const dropdownNameUntilEllipsisLen = 25;

    context('when only the unassigned project is selected', () => {
      before(() => {
        localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify([
          {label: unassigned, value: unassigned, checked: true},
          {label: project1Name, value: project1ID, checked: false},
          {label: project2Name, value: project2ID, checked: false},
          {label: unusedProject, value: unusedProject, checked: false}
        ]));

        // relog to apply localstorage state
        cy.adminLogin('/settings/teams').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminToken = admin.id_token;
        });
      });

      it('can create a team with no projects (unassigned) ' +
      'and cannot access projects dropdown', () => {
        cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
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
        cy.go('back');
      });
    });

    context('when there are multiple custom projects selected in the ' +
      'filter (including the unassinged project)', () => {
      before(() => {
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

        localStorage.setItem(STORE_OPTIONS_KEY, JSON.stringify([
          {label: unassigned, value: unassigned, checked: true},
          {label: project1Name, value: project1ID, checked: true},
          {label: project2Name, value: project2ID, checked: true},
          {label: unusedProject, value: unusedProject, checked: false}
        ]));

        // relog to apply localstorage state
        cy.adminLogin('/settings/teams').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminToken = admin.id_token;
        });
      });

      after(() => {
        cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
      });

      beforeEach(() => {
        // TODO (tc): need to force reload because ngrx/store cache can get
        // in some wacko states when the API returns no conflict but it
        // has team in the cache from previous run. it does not properly
        // overwrite projects array. is this a bug in EntityAdapter.addOne?
        // super unlikely condition in a non-test setting but maybe worth investigating.
        cy.reload(true);
        cy.get('app-welcome-modal').invoke('hide');
      });

      afterEach(() => {
        cy.cleanupTeamsByDescriptionPrefix(adminToken, cypressPrefix);
      });

      it('can create a team with mulitple projects', () => {
        const projectSummary = '2 projects';
        cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
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
        cy.go('back');
      });

      it('can create a team with one project selected', () => {
        cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
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
        cy.go('back');
      });

      it('can create a team with no projects selected (unassigned)', () => {
        cy.get('app-team-management chef-toolbar chef-button').contains('Create Team').click();
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
        cy.go('back');
      });
    });
  });

  it('displays team details for admins team', () => {
    cy.get('chef-td').contains('admins').click();

    cy.get('chef-breadcrumbs').contains('Teams');
    cy.get('chef-breadcrumbs').contains('admins');

    cy.get('.page-title').contains('admins');
    cy.contains('Add User');
    cy.go('back');
  });

  it('displays team users for admins team', () => {
    cy.get('chef-td').contains('admins').click();
    cy.get('chef-option').contains('Users');
    cy.get('app-user-table chef-th').contains('Name');
    cy.get('app-user-table chef-th').contains('Username');
    cy.get('app-user-table chef-td').contains('Local Administrator');
    cy.get('app-user-table chef-td').contains('admin');
    cy.go('back');
  });
});
