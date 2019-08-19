// Cypress Commands: any action that could be taken in any test

Cypress.Commands.add('login', (url: string, username: string) => {
  // CYPRESS_BASE_URL environment variable must be set
  cy.visit(url);

  // only environments using SAML or LDAP present this login method selection
  return cy.location('pathname')
    .then((path: any) => path.startsWith('/dex/auth/local'))
    .then((local: any) => {
      if (local) {
        LoginHelper(username);
      } else {
        cy.get('a').contains('Sign in as a local user').click().then(() => LoginHelper(username));
      }
    });
});

Cypress.Commands.add('adminLogin', (url: string) => {
  // CYPRESS_BASE_URL environment variable must be set
  return cy.login(url, 'admin');
});

Cypress.Commands.add('logout', () => {
  cy.get('[data-cy=user-profile-button]').click();
  cy.get('[data-cy=sign-out-button]').click();
  return cy.url().should('include', '/dex/auth');
});

// applyProjectsFilter will deselect any selected projects from the filter,
// check any projects by NAME (not id) passed -- if any -- and apply any changes.
// if there are no resulting changes to apply, it will simply click out of the filter.
Cypress.Commands.add('applyProjectsFilter', (projectsToFilterOn: string[]) => {
  cy.get('app-projects-filter button#projects-filter-button').click();
  cy.get('app-projects-filter chef-dropdown#projects-filter-dropdown')
    .find('chef-checkbox').each(child => {
    // deselect every checkbox
    if (child.attr('aria-checked') === 'true') {
      child.click();
    }
  });

  // check all desired checkboxes
  projectsToFilterOn.forEach(proj =>
    cy.get(`app-projects-filter chef-checkbox[title="${proj}"]`).find('chef-icon').click());

  if (projectsToFilterOn.length === 0) {
    // no changes to apply, close projects filter
    cy.get('app-projects-filter button#projects-filter-button').click();
  } else {
    // apply projects filter
    cy.get('app-projects-filter chef-button#projects-filter-apply-changes').click();
  }
});

interface MemoryMap {
  [key: string]: any;
}

const LOCAL_STORAGE_MEMORY: MemoryMap = {};
const SESSION_MEMORY: MemoryMap = {};

Cypress.Commands.add('saveStorage', () => {
  Object.keys(localStorage).forEach(key => {
    LOCAL_STORAGE_MEMORY[key] = localStorage[key];
  });
  Object.keys(sessionStorage).forEach(key => {
    SESSION_MEMORY[key] = sessionStorage[key];
  });
});


Cypress.Commands.add('restoreStorage', () => {
  Object.keys(LOCAL_STORAGE_MEMORY).forEach(key => {
    localStorage.setItem(key, LOCAL_STORAGE_MEMORY[key]);
  });
  Object.keys(SESSION_MEMORY).forEach(key => {
    sessionStorage.setItem(key, SESSION_MEMORY[key]);
  });

  cy.server();
  // mock refresh token call in case it fails
  const user = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
  cy.route({
    method: 'GET',
    url: '**/session/refresh',
    status: 200,
    response: {
      id_token: user.id_token
    }
  });
});

Cypress.Commands.add('generateAdminToken', (idToken: string) => {
  const adminTokenObj = {
    id: 'cypress-api-test-admin-token',
    name: 'cypress-api-test-admin-token'
  };

  // cleanup token from previous test
  cy.request({
    auth: { bearer: idToken },
    method: 'DELETE',
    url: '/apis/iam/v2beta/tokens/cypress-api-test-admin-token',
    failOnStatusCode: false
  });

  cy.request({
    auth: { bearer: idToken },
    method: 'POST',
    url: '/apis/iam/v2beta/tokens',
    body: adminTokenObj
  }).then((response: Cypress.ObjectLike) => {
    Cypress.env('adminTokenValue', response.body.token.value);
  });
  cy.request({
    auth: { bearer: idToken },
    method: 'POST',
    url: '/apis/iam/v2beta/policies/administrator-access/members:add',
    body: {
      members: [`token:${adminTokenObj.id}`]
    }
  });
});

Cypress.Commands.add('cleanupPoliciesByIDPrefix', (idToken: string, idPrefix: string) => {
  cy.request({
    auth: { bearer: idToken },
    method: 'GET',
    url: '/apis/iam/v2beta/policies',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const policy of body.policies) {
      if (policy.id.startsWith(idPrefix)) {
        cy.request({
          auth: { bearer: idToken },
          method: 'DELETE',
          url: `/apis/iam/v2beta/policies/${policy.id}`,
          failOnStatusCode: false
        });
      }
    }
  });
});

Cypress.Commands.add('cleanupUsersByNamePrefix', (idToken: string, namePrefix: string) => {
  cy.request({
    auth: { bearer: idToken },
    method: 'GET',
    url: '/api/v0/auth/users',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const user of body.users) {
      if (user.name.startsWith(namePrefix)) {
        cy.request({
          auth: { bearer: idToken },
          method: 'DELETE',
          url: `/api/v0/auth/users/${user.username}`,
          failOnStatusCode: false
        });
      }
    }
  });
});

Cypress.Commands.add('cleanupProjectsByIDPrefix', (idToken: string, idPrefix: string) => {
  cy.request({
    auth: { bearer: idToken },
    method: 'GET',
    url: '/apis/iam/v2beta/projects',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const project of body.projects) {
      if (project.id.startsWith(idPrefix)) {
        cy.request({
          auth: { bearer: idToken },
          method: 'DELETE',
          url: `/apis/iam/v2beta/projects/${project.id}`,
          failOnStatusCode: false
        });
      }
    }
  });
});


// helpers

function LoginHelper(username: string) {
  cy.url().should('include', '/dex/auth/local');
  cy.server();
  cy.route('POST', '/api/v0/auth/introspect_some').as('getAuth');

  // login
  cy.get('#login').type(username);
  cy.get('#password').type('chefautomate');

  cy.get('[type=submit]').click().then(() => {
    expect(localStorage.getItem('chef-automate-user')).to.contain(username);

    // close welcome modal if present
    cy.get('app-welcome-modal').invoke('hide');
    cy.saveStorage();

    cy.wait('@getAuth');
  });
}

Cypress.Commands.add('cleanupTeamsByDescriptionPrefix', (idToken: string, namePrefix: string) => {
  cy.request({
    auth: { bearer: idToken },
    method: 'GET',
    url: '/api/v0/auth/teams',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const team of body.teams) {
      if (team.description.startsWith(namePrefix)) {
        cy.request({
          auth: { bearer: idToken },
          method: 'DELETE',
          url: `/api/v0/auth/teams/${team.id}`,
          failOnStatusCode: false
        });
      }
    }
  });
});

