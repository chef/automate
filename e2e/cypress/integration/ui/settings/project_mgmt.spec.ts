import { itFlaky } from '../../../support/constants';
interface Project {
  id: string;
  name: string;
  type: string;
}

describe('project management', () => {
  // we increase the default delay to mimic the average human's typing speed
  // only need this for input values upon which later test assertions depend
  // ref: https://github.com/cypress-io/cypress/issues/534
  const typeDelay = 50;
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress-test';
  const projectID = `${cypressPrefix}-project1-${now}`;
  const customProjectID = `${cypressPrefix}-customproject-${now}`;
  const projectName = `${cypressPrefix} project1 ${now}`;
  const ruleID = `${cypressPrefix}-rule-${now}`;
  const ruleName = `${cypressPrefix} rule ${now}`;

  before(() => {
    cy.adminLogin('/settings/projects').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
      // Set the projects filter to all projects
      cy.applyProjectsFilter([]);
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
  });

  it('displays a list of projects', () => {
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: '/apis/iam/v2/projects'
    }).then((response) => {
      expect(response.status).to.equal(200);
      response.body.projects.map((project: Project) => project.id)
        .forEach((id: string) => {
          cy.get('chef-table-new chef-table-cell').contains(id);
        });
    });
  });

  it('can create a project without adding a custom ID', () => {
    cy.get('[data-cy=create-project]').contains('Create Project').click();
    cy.get('app-project-list chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').focus()
      .type(projectName, { delay: typeDelay }).should('have.value', projectName);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');
    cy.get('#id-input').should('have.value', projectID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-project-list chef-modal').should('not.be.visible');
    cy.get('#main-content-wrapper').scrollTo('top');
    cy.get('chef-notification.info').should('be.visible');
    cy.contains(projectName).should('exist');
    cy.contains(projectID).should('exist');

    cy.url().should('include', '/settings/projects');
  });

  it('can open the new project\'s details page', () => {
    cy.get('[data-cy=project-details]').contains(projectName).click();
    cy.url().should('include', `/settings/projects/${projectID}`);
  });

  // something is occasionally resetting the projects filter to (unassigned)
  itFlaky('can create a rule for the new project', () => {
    cy.get('app-project-details').contains('Edits are pending').should('not.exist');
    cy.get('app-project-details app-authorized button').contains('Create Rule').click();

    cy.url().should('include', `/settings/projects/${projectID}/rules`);
    cy.get('app-project-rules chef-page').should('be.visible');

    cy.get('#create-name input').focus().type(ruleName, { delay: typeDelay })
      .should('have.value', ruleName);
    cy.get('[data-cy=edit-id]').click();

    cy.get('#create-id input').should('have.value', ruleID);

    // Verify correct attributes/operators are selectable for each resource
    // Event
    cy.get('#create-type-dropdown').select('Event');
    cy.get('[data-cy=attribute-dropdown]').select('Chef Organization');
    cy.get('[data-cy=attribute-dropdown]').select('Chef Server');
    cy.get('[data-cy=operator-dropdown]').select('equals');
    cy.get('[data-cy=operator-dropdown]').select('member of');

    // Node
    cy.get('#create-type-dropdown').select('Node');
    const nodeAttributes = ['Chef Organization', 'Chef Server', 'Environment',
      'Chef Role', 'Chef Tag', 'Chef Policy Name', 'Chef Policy Group'];

    nodeAttributes.forEach((att: string) => {
      cy.get('[data-cy=attribute-dropdown]').select(att);
    });

    cy.get('[data-cy=operator-dropdown]').select('member of');
    cy.get('[data-cy=operator-dropdown]').select('equals');
    cy.get('[data-cy=rule-value] input').type('Chef Policy Group Foo');

    cy.get('app-project-rules #right-buttons button').contains('Save Rule').click();
    cy.get('app-project-rules chef-page').should('not.be.visible');

    cy.url().should('include', `/settings/projects/${projectID}`);
    cy.get('app-project-details chef-table-cell').contains(ruleID);
    cy.get('app-project-details').contains('Edits are pending');
  });

  itFlaky('displays a list of rules for a project', () => {
    ['Name', 'ID', 'Resource Type', 'Conditions', 'Edits'].forEach((header) => {
      cy.get('app-project-details chef-table-header-cell').contains(header);
    });

    cy.get('app-project-details chef-table-cell').contains(ruleID);
    cy.get('app-project-details chef-table-cell').contains(ruleName);
    cy.get('app-project-details chef-table-cell').contains('Node');
    cy.get('app-project-details chef-table-cell').contains('1 condition');
    cy.get('app-project-details chef-table-cell').contains('Edits pending');
  });

  itFlaky('can update a project rule', () => {
    cy.get('app-project-details').contains('Edits are pending');
    const updatedRuleName = `updated ${ruleName}`;
    cy.get('app-project-details a').contains(ruleName).click();

    cy.url().should('include', `/settings/projects/${projectID}/rules/${ruleID}`);
    cy.get('app-project-rules chef-page').should('be.visible');

    cy.get('#create-name input').focus().clear().type(updatedRuleName, { delay: typeDelay })
      .should('have.value', updatedRuleName);

    cy.get('#create-id input').should('have.value', ruleID);
    cy.get('#create-id input').should('be.disabled');

    cy.get('#create-type-dropdown option:first').should('have.value', 'NODE');
    cy.get('#create-type-dropdown option:first').should('be.selected');
    cy.get('#create-type-dropdown').should('be.disabled');

    cy.get('app-project-rules chef-button').contains('Add Condition').click();
    cy.get('app-project-rules .end-row-items span').contains('AND').should('be.visible');

    cy.get('[data-cy=attribute-dropdown]:last')
      .select('Environment');

    cy.get('[data-cy=operator-dropdown]:last')
      .select('member of');

    cy.get('[data-cy=rule-value]:last input')
      .type('Dev, Prod, Test');

    cy.get('app-project-rules #right-buttons button').contains('Save Rule').click();
    cy.get('app-project-rules chef-page').should('not.be.visible');

    cy.url().should('include', '/settings/projects');
    cy.get('app-project-details chef-table-cell').contains(updatedRuleName);
    cy.get('app-project-details chef-table-cell').contains('2 conditions');
    cy.get('app-project-details').contains('Edits are pending');
  });

  itFlaky('can update a project name', () => {
    const updatedProjectName = `updated ${projectName}`;
    cy.route('GET', `/apis/iam/v2/projects/${projectID}`).as('getProject');

    cy.get('[data-cy=details-tab]').click();
    cy.wait('@getProject');

    cy.get('[data-cy=update-project-name]').focus().clear()
      .type(updatedProjectName, { delay: typeDelay })
      .should('have.value', updatedProjectName);
    cy.get('app-project-details chef-button').contains('Save').click();

    cy.get('app-project-details h1.page-title').contains(updatedProjectName);
  });

  itFlaky('can delete a project rule', () => {
    cy.get('app-project-details').contains('Edits are pending');
    cy.get('[data-cy=rules-tab]').click();

    cy.get('app-project-details chef-table-cell').contains(ruleID).parent()
      .find('mat-select').as('controlMenu');
    // we throw in a should so cypress retries until introspection allows menu to be shown
    cy.get('@controlMenu').should('be.visible')
      .click();
    cy.get('@controlMenu').find('[data-cy=delete-rule]').click({ force: true });

    cy.get('app-project-details chef-button').contains('Delete Rule').click();

    // since this is a cypress custom project, we know this is the only rule.
    // the empty UI should show up so entire table will be missing.
    cy.get('app-project-details chef-table-body').should('not.exist');
    cy.get('app-project-details').contains('Edits are pending').should('not.exist');
  });

  // sometimes the deleted successfully notification isn't showing up
  itFlaky('can delete a project', () => {
    cy.get('.breadcrumb').click();

    cy.get('app-project-list chef-table-cell').contains(projectID).parent()
      .find('mat-select').as('controlMenu');
    cy.get('@controlMenu').click({ force: true });
    cy.get('@controlMenu').find('[data-cy=delete-project]').click({ force: true });

    cy.get('app-project-list chef-button').contains('Delete Project').click();

    // Once we get this notification we know the network call to delete succeeded,
    // so now we can check if there are other projects or not.
    cy.get('#main-content-wrapper').scrollTo('top');
    cy.get('chef-notification.info').contains(`Deleted project ${projectID}`);
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: '/apis/iam/v2/projects'
    }).then((response) => {
      expect(response.status).to.equal(200);
      // no projects are left so we shouldn't render the table at all
      if (response.body.projects.length === 0) {
        cy.get('app-project-list chef-table-body').should('not.exist');
        // otherwise, check that the projectID is no longer in the table
      } else {
        cy.get('app-project-list chef-table-body chef-table-cell')
          .contains(projectID).should('not.exist');
      }
    });
  });

  // these tests are currently interdependent so mark as flaky until the above isn't
  itFlaky('can create a project with a custom ID', () => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    cy.get('[data-cy=create-project]').contains('Create Project').click();
    cy.get('app-project-list chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').focus()
      .type(projectName, { delay: typeDelay }).should('have.value', projectName);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');

    cy.get('[data-cy=create-id]').should('be.visible').clear()
      .type(customProjectID, { delay: typeDelay }).should('have.value', customProjectID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-project-list chef-modal').should('not.be.visible');
    cy.get('#main-content-wrapper').scrollTo('top');
    cy.get('chef-notification.info').should('be.visible');
    cy.contains(projectName).should('exist');
    cy.contains(customProjectID).should('exist');

    cy.url().should('include', '/settings/projects');
  });
});
