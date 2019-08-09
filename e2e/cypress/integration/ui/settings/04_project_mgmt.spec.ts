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
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;

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
    // should see "Create the first ingest rule to get started"
    // click "Create Rule" button
    // should open rule detail
    // type rule name
    // select resource type
    // click add condition
    // select node attribute, operator, value
    // click save rule
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
