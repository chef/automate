import { Project } from '../../../support/types';

describe('project rule management', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'project-rules';
  const project: Project = {
    id: `${cypressPrefix}-proj-${now}`,
    name: 'project rules test proj',
    skip_policies: true
  };
  const ruleID = `${cypressPrefix}-rule-${now}`;
  const ruleName = `${cypressPrefix} rule ${now}`;

  before(() => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/projects',
      body: project
    }).then((resp) => {
      expect(resp.status).to.equal(200);
    });

    cy.adminLogin(`/settings/projects/${project.id}`);
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

  it('can create a rule for the new project', () => {
    cy.get('app-project-details app-authorized button').contains('Create Rule').click();

    cy.url().should('include', `/settings/projects/${project.id}/rules`);
    cy.get('app-project-rules chef-page').should('be.visible');

    cy.get('#create-name input').focus().type(ruleName)
      .should('have.value', ruleName);
    cy.get('[data-cy=edit-id]').click();

    cy.get('#create-id input').should('have.value', ruleID);

    // Verify correct attributes/operators are selectable for each resource
    cy.get('#create-type-dropdown').select('Event');
    cy.get('[data-cy=attribute-dropdown]').select('Chef Organization');
    cy.get('[data-cy=attribute-dropdown]').select('Chef Infra Server');
    cy.get('[data-cy=operator-dropdown]').select('equals');
    cy.get('[data-cy=operator-dropdown]').select('member of');

    cy.get('#create-type-dropdown').select('Node');
    const nodeAttributes = ['Chef Organization', 'Chef Infra Server', 'Environment',
      'Chef Role', 'Chef Tag', 'Chef Policy Name', 'Chef Policy Group'];

    nodeAttributes.forEach((att: string) => {
      cy.get('[data-cy=attribute-dropdown]').select(att);
    });

    cy.get('[data-cy=operator-dropdown]').select('member of');
    cy.get('[data-cy=operator-dropdown]').select('equals');
    cy.get('[data-cy=rule-value]').type('Chef Policy Group Foo');

    cy.get('app-project-rules #right-buttons button').contains('Save Rule').click();
    cy.get('app-project-rules chef-page').should('not.be.visible');

    cy.url().should('include', `/settings/projects/${project.id}`);
    cy.get('app-project-details chef-td').contains(ruleID);
    cy.get('app-pending-edits-bar').contains('Project edits pending');

    // verify success notification and then dismiss it
    cy.get('app-notification.info').contains(`Created rule ${ruleName}`);
    cy.get('app-notification.info chef-icon').click();
  });

  it('displays a list of rules for a project', () => {
    ['Name', 'ID', 'Resource Type', 'Conditions', 'Edits'].forEach((header) => {
      cy.get('app-project-details chef-th').contains(header);
    });

    cy.get('app-project-details chef-td').contains(ruleID);
    cy.get('app-project-details chef-td').contains(ruleName);
    cy.get('app-project-details chef-td').contains('Node');
    cy.get('app-project-details chef-td').contains('1 condition');
    cy.get('app-project-details chef-td').contains('Edits pending');
  });

  it('can update a project rule', () => {
    cy.get('app-pending-edits-bar').contains('Project edits pending');
    const updatedRuleName = `updated ${ruleName}`;
    cy.get('app-project-details a').contains(ruleName).click();

    cy.url().should('include', `/settings/projects/${project.id}/rules/${ruleID}`);
    cy.get('app-project-rules chef-page').should('be.visible');

    cy.get('#create-name input').focus().clear().type(updatedRuleName)
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

    cy.get('[data-cy=rule-value]:last')
      .type('Dev, Prod, Test');

    cy.get('app-project-rules #right-buttons button').contains('Save Rule').click();
    cy.get('app-project-rules chef-page').should('not.be.visible');

    cy.url().should('include', '/settings/projects');
    cy.get('app-project-details chef-td').contains(updatedRuleName);
    cy.get('app-project-details chef-td').contains('2 conditions');
    cy.get('app-pending-edits-bar').contains('Project edits pending');
  });

  it('can delete a project rule', () => {
    cy.get('app-pending-edits-bar').contains('Project edits pending');
    cy.get('[data-cy=rules-tab]').click();

    cy.get('app-project-details chef-td').contains(ruleID).parent()
      .find('.mat-select-trigger').as('controlMenu');

    // we throw in a `should` so cypress retries until introspection allows menu to be shown
    cy.get('@controlMenu').should('be.visible')
      .click();
    cy.get('[data-cy=delete-rule]').should('be.visible')
      .click();

    // accept dialog
    cy.get('app-project-details chef-button').contains('Delete Rule').click();

    // since this is a cypress custom project, we know this is the only rule.
    // the empty UI should show up so entire table will be missing.
    cy.get('app-project-details chef-tbody').should('not.exist');
    cy.get('app-pending-edits-bar').contains('Project edits pending').should('not.be.visible');

    // verify success notification and then dismiss it
    cy.get('app-notification.info').contains(`Deleted rule ${ruleID}`);
    cy.get('app-notification.info chef-icon').click();
  });
});

