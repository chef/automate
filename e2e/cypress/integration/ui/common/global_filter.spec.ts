import { Project } from '../../../support/types';

describe('global projects filter', () => {
  const proj1: Project = {
    id: `cypress-project-1-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'Cypress Project 1',
    skip_policies: true
  };
  const proj2: Project = {
    id: `cypress-project-2-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'Cypress Project 2',
    skip_policies: true
  };
  const proj3: Project = {
    id: `cypress-project-3-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'Cypress Project 3',
    skip_policies: true
   };
  const pol_id = `cypress-policy-${Cypress.moment().format('MMDDYYhhmm')}`;
  const nonAdminUsername = `nonadmin-${Cypress.moment().format('MMDDYYhhmm')}`;

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cleanupProjects(admin.id_token);
      createProject(admin.id_token, proj1);
      createProject(admin.id_token, proj2);
      createProject(admin.id_token, proj3);

      createUser(admin.id_token, nonAdminUsername);
      createPolicy(admin.id_token, pol_id, nonAdminUsername, [proj1.id, proj2.id]);
      cy.logout();
    });
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

function cleanupProjects(id_token: string): void {
  cy.request({
    auth: { bearer: id_token },
    method: 'GET',
    url: '/apis/iam/v2/projects',
    failOnStatusCode: false
  }).then((resp) => {
    for (const project of resp.body.projects) {
      cy.request({
        auth: { bearer: id_token },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${project.id}`,
        failOnStatusCode: false
      });
    }
  });
}

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
