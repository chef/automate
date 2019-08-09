import forEach = require("cypress/types/lodash/forEach");

interface Project {
  id: string;
  name: string;
  type: string;
}

// assume 2.0 if not in CI. if you wish something different start cypress with
// IAM_VERSION set to what you are testing.
if (Cypress.env('IAM_VERSION') === undefined) {
  Cypress.env('IAM_VERSION', 'v2.0');
}

const describeIAMV2P1 = Cypress.env('IAM_VERSION') === 'v2.1' ? describe : describe.skip;

describeIAMV2P1('project management', () => {
  let adminToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress-test';
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const ruleID = `${cypressPrefix}-rule-${now}`;
  const ruleName = `${cypressPrefix} rule ${now}`;

  before(() => {
    cy.adminLogin('/settings/projects').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;
      cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });


  it('list projects', () => {
    let projectIDs: string[];
    cy.request({
      auth: { bearer: adminToken },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((response) => {
      expect(response.status).to.equal(200);
      projectIDs = response.body.projects.map((project: Project) => project.id);

      projectIDs.forEach((id: string) => {
        cy.get('chef-table chef-td').contains(id);
      });
    });
  });

  it('can create a project', () => {
    cy.get('#create-button').contains('Create Project').click();
    cy.get('app-create-object-modal chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').type(project1Name);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');
    cy.get('#id-input').should('have.value', project1ID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-create-object-modal chef-modal').should('not.be.visible');
    cy.get('chef-notification.info').should('be.visible');
    cy.contains(project1Name).should('exist');
    cy.contains(project1ID).should('exist');

    cy.url().should('include', `/settings/projects/${project1ID}`);
  });

  it('can create a rule for the new project', () => {
    cy.get('app-authorized button').contains('Create Rule').click();

    cy.url().should('include', `/settings/projects/${project1ID}/rules`);
    cy.get('chef-page').should('be.visible');

    cy.get('#create-name input').type(ruleName)
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
        'Chef Role', 'Chef Tag', 'Chef Policy Name', 'Chef Policy Group']

    nodeAttributes.forEach((att: string) => {
        cy.get('[data-cy=attribute-dropdown]').select(att);
    });

    cy.get('[data-cy=operator-dropdown]').select('member of');
    cy.get('[data-cy=operator-dropdown]').select('equals');
    cy.get('[data-cy=rule-value] input').type('Chef Policy Group Foo');

    cy.get('#right-buttons button').contains('Save Rule').click();
    cy.get('chef-page').should('not.be.visible')

    cy.url().should('include', `/settings/projects/${project1ID}`);
    cy.get('chef-td').contains(ruleID)
  });

  it('can see a list of rules for a project', () => {
  });

  it('can update a project rule', () => {
  });

  it('can delete a project rule', () => {
  });

  it('can update a project name', () => {
  });

  it('delete a project', () => {
  });
});
