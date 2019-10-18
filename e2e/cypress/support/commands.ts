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
      child.trigger('click');
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

Cypress.Commands.add('generateAdminToken', (idToken: string) => {
  const adminTokenObj = {
    id: 'cypress-api-test-admin-token',
    name: 'cypress-api-test-admin-token'
  };

  // delete old token if exists
  cy.request({
    auth: { bearer: idToken },
    method: 'DELETE',
    url: `/apis/iam/v2beta/tokens/${adminTokenObj.id}`,
    failOnStatusCode: false
  }).then((resp: Cypress.ObjectLike) => {
    expect([200, 404]).to.include(resp.status);
  });

  // create token
  cy.request({
    auth: { bearer: idToken },
    method: 'POST',
    url: '/apis/iam/v2beta/tokens',
    body: adminTokenObj
  }).then((resp: Cypress.ObjectLike) => {
    Cypress.env('ADMIN_TOKEN', resp.body.token.value);
  });

  // grant permissions based on IAM version
  if (Cypress.env('IAM_VERSION') === 'v1') {
    cy.request({
      auth: { bearer: idToken },
      method: 'POST',
      url: '/apis/iam/v2beta/policies',
      body: {
        action: '*',
        resource: '*',
        subjects: [`token:${adminTokenObj.id}`]
      }
    });
  } else {
    cy.request({
      auth: { bearer: idToken },
      method: 'POST',
      url: '/apis/iam/v2beta/policies/administrator-access/members:add',
      body: {
        members: [`token:${adminTokenObj.id}`]
      }
    });
  }

  waitUntilAdminTokenPermissioned(100);
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
  // mock refresh token call in case it fails and forces logout
  const user = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
  cy.route({
    method: 'GET',
    url: '**/session/refresh',
    status: 200,
    response: {
      id_token: user.id_token
    }
  });

  if (Cypress.env('IAM_VERSION') === 'v2.1') {
    // mock background polling since it's frequent and can interfere with loading wait time
    cy.route({
      method: 'GET',
      url: '**/apply-rules',
      status: 200,
      response: {
        state: 'not_running',
        estimated_time_complete: '0001-01-01T00:00:00Z',
        percentage_complete: 1,
        failed: false,
        failure_message: ''
      }
    });
  }
});

Cypress.Commands.add('cleanupV2IAMObjectsByIDPrefixes',
  (idPrefix: string, iamObjects: string[]) => {

  iamObjects.forEach((iamObject) => {
    cleanupV2IAMObjectByIDPrefix(idPrefix, iamObject);
  });
});

function cleanupV2IAMObjectByIDPrefix(idPrefix: string, iamObject: string): void {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: `/apis/iam/v2beta/${iamObject}`
  }).then((resp) => {
    for (const object of resp.body[iamObject]) {
      if (object.id.startsWith(idPrefix)) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'DELETE',
          url: `/apis/iam/v2beta/${iamObject}/${object.id}`
        }).then((deleteResp) => {
          expect([200, 404]).to.include(deleteResp.status);
        });
      }
    }
  });
}

Cypress.Commands.add('cleanupUsersByNamePrefix', (namePrefix: string) => {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: '/api/v0/auth/users',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const user of body.users) {
      if (user.name.startsWith(namePrefix)) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'DELETE',
          url: `/api/v0/auth/users/${user.username}`,
          failOnStatusCode: false
        });
      }
    }
  });
});

Cypress.Commands.add('cleanupTeamsByDescriptionPrefix', (namePrefix: string) => {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: '/api/v0/auth/teams',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const team of body.teams) {
      if (team.description.startsWith(namePrefix)) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'DELETE',
          url: `/api/v0/auth/teams/${team.id}`,
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
  // the gloabl permissions for the user that populates the initial permissions cache
  cy.route('GET', '/api/v0/auth/introspect').as('getAuthPopulateCache');
  // the parameterized permissions, called for the specific page loaded
  cy.route('POST', '/api/v0/auth/introspect').as('getAuthParameterized');

  // login
  cy.get('#login').type(username);
  cy.get('#password').type('chefautomate');

  cy.get('[type=submit]').click().then(() => {
    expect(localStorage.getItem('chef-automate-user')).to.contain(username);

    // close welcome modal if present
    cy.get('app-welcome-modal').invoke('hide');
    cy.saveStorage();

    cy.wait(['@getAuthPopulateCache', '@getAuthParameterized']);
  });
}

function waitUntilAdminTokenPermissioned(attempts: number): void {
  if (attempts === -1) {
    throw new Error('admin token failed to generate');
  }
  // admin-only API endpoint
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: '/apis/iam/v2beta/projects',
    method: 'GET',
    failOnStatusCode: false
  }).then((response) => {
    if (response.status === 200) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for admin token to be permissioned`);
      cy.wait(1000);
      waitUntilAdminTokenPermissioned(--attempts);
    }
  });
}
