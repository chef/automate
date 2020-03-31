interface CreateProject {
  id: string;
  name: string;
}

describe('global projects filter', () => {
  const proj1 = <CreateProject>
    { id: 'cypress-project-1', name: 'Cypress Project 1 ' + Cypress.moment().format('MMDDYYhhmm') };
  const proj2 = <CreateProject>
    { id: 'cypress-project-2', name: 'Cypress Project 2 ' + Cypress.moment().format('MMDDYYhhmm')};
  const proj3 = <CreateProject>
    { id: 'cypress-project-3', name: 'Cypress Project 3 ' + Cypress.moment().format('MMDDYYhhmm') };
  // TODO uncomment with non-admin test
  // const pol_id = "cypress-policy"
  // const nonAdminUsername = "nonadmin"

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cleanupProjects(admin.id_token);
      createProject(admin.id_token, proj1);
      createProject(admin.id_token, proj2);
      createProject(admin.id_token, proj3);
      // TODO uncomment with non-admin test/ move up project creation
      // cy.createUser(admin.id_token, nonAdminUsername)
      // cy.createPolicy(admin.id_token, pol_id, nonAdminUsername, [proj1, proj2])
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
    cy.logout();
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

function createPolicy(id_token: string, id: string, username: string, projects: string): void {
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

function createProject(id_token: string, project: CreateProject): void {
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
