import { Project } from '../../../support/types';

describe('global projects filter', () => {
  const cypressPrefix = 'cypress-global-filter';
  const now = Cypress.moment().format('MMDDYYhhmm');

  const proj1: Project = {
    id: `${cypressPrefix}-proj1-${now}`,
    name: 'Cypress Project 1',
    skip_policies: true
  };
  const proj2: Project = {
    id: `${cypressPrefix}-proj2-${now}`,
    name: 'Cypress Project 2',
    skip_policies: true
  };
  const proj3: Project = {
    id: `${cypressPrefix}-proj3-${now}`,
    name: 'Cypress Project 3',
    skip_policies: true
   };
  const pol_id = `${cypressPrefix}-pol-${now}`;
  const nonAdminUsername = `${cypressPrefix}-nonadmin-${now}`;

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      createProject(admin.id_token, proj1);
      createProject(admin.id_token, proj2);
      createProject(admin.id_token, proj3);

      createUser(admin.id_token, nonAdminUsername);
      createPolicy(admin.id_token, pol_id, nonAdminUsername, [proj1.id, proj2.id]);
      cy.logout();
    });
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'users', 'policies']);
  });

  it('shows all projects for admin', () => {
    cy.adminLogin('/settings');
    cy.get('chef-sidebar');
    const allowedProjects = [proj1.name, proj2.name, proj3.name, '(unassigned)'];
    // we don't check that projects in dropdown match *exactly* as
    // we can't control creation of other projects in the test env
    cy.get('.dropdown-label').click();
    allowedProjects.forEach(project => {
      cy.get('#projects-filter-dropdown').contains(project);
    });
  });

  it('shows only allowed projects for non-admin', () => {
    cy.logout();
    cy.login('/settings', nonAdminUsername);
    cy.get('chef-sidebar');
    const allowedProjects = [proj1.name, proj2.name];

    cy.get('.dropdown-label').click();
    allowedProjects.forEach(project => {
      cy.get('#projects-filter-dropdown').contains(project);
    });
  });
});

function createUser(id_token: string, username: string): void {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2/users',
    failOnStatusCode: false,
    body: {
      id: username,
      name: 'cypress test user',
      password: 'chefautomate'
    }
  });
}

function createPolicy(id_token: string, id: string, username: string, projects: string[]): void {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2/policies',
    failOnStatusCode: false,
    body: {
      id,
      name: 'non-admin policy',
      members: ['user:local:' + username],
      statements: [
        {
          effect: 'ALLOW',
          actions: ['iam:teams:list', 'iam:teams:get'],
          projects
        }
      ]
    }
  }).then((response) => {
    expect(response.status).to.be.oneOf([200, 404]);
  });
}

function createProject(id_token: string, project: Project): void {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2/projects',
    failOnStatusCode: false,
    body: project
  }).then((response) => {
    expect(response.status).to.be.oneOf([200, 404]);
  });
}
