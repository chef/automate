describe('project management', () => {
  let adminToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'cypress-test';
  const project1ID = `${cypressPrefix}-project1-${now}`;
  const project1Name = `${cypressPrefix} project1 ${now}`;
  const project2ID = `${cypressPrefix}-project2-${now}`;
  const project2Name = `${cypressPrefix} project2 ${now}`;

  let iamVersion = <string><Object>Cypress.env('IAM_VERSION');
  // assume 2.0 if not in CI. if you wish something different start cypress with
  // IAM_VERSION set to what you are testing.
  if (iamVersion === undefined) {
    iamVersion = 'v2.0';
  }

  const describeIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
  const describeProjectsEnabled = iamVersion === 'v2.1' ? describe : describe.skip;

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

  it('can create a project', () => {
    cy.get('#create-button').contains('Create Project').click();
    cy.get('app-create-object-modal chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').type(project1Name);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');
    cy.get('[data-cy=create-id]').should('be.visible').clear().type(project1ID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-create-object-modal chef-modal').should('not.be.visible');
    cy.get('chef-notification.info').should('be.visible');
    cy.contains(project1Name).should('exist');
    cy.contains(project1ID).should('exist');
      // should see "Create the first ingest rule to get started"
      // click "Create Rule" button
      // should open rule detail
      // type rule name
      // select resource type
      // click add condition
      // select node attribute, operator, value
      // click save rule

  });

  it('list projects', () => {
  });

  it('delete a project', () => {
  });

  it('can create a project with a rule', () => {
    // or add rule to project?
  });

  it('can see a list of rules for a project', () => {
  });

  it('can update a project rule', () => {
  });

  it('can delete a project rule', () => {
  });

  it('can update a project name', () => {
  });
});
