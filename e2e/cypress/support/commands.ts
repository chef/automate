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
    url: `/apis/iam/v2/tokens/${adminTokenObj.id}`,
    failOnStatusCode: false
  }).then((resp: Cypress.ObjectLike) => {
    expect(resp.status).to.be.oneOf([200, 404]);
  });

  // create token (OK to run v2 endpoints even if system is v1)
  cy.request({
    auth: { bearer: idToken },
    method: 'POST',
    url: '/apis/iam/v2/tokens',
    body: adminTokenObj
  }).then((resp: Cypress.ObjectLike) => {
    Cypress.env('ADMIN_TOKEN', resp.body.token.value);
  });

  cy.request({
    auth: { bearer: idToken },
    method: 'POST',
    url: '/apis/iam/v2/policies/administrator-access/members:add',
    body: {
      members: [`token:${adminTokenObj.id}`]
    }
  });

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
});

Cypress.Commands.add('cleanupV2IAMObjectsByIDPrefixes',
  (idPrefix: string, iamObjects: string[]) => {

  iamObjects.forEach((iamObject) => {
    if (iamObject === 'projects') {
      cleanupProjectsByIDPrefixes(idPrefix);
    } else {
      cleanupV2IAMObjectByIDPrefix(idPrefix, iamObject);
    }
  });
});

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
    url: '/apis/iam/v2/teams',
    failOnStatusCode: false
  }).then((resp) => {
    const body = resp.body;
    for (const team of body.teams) {
      if (team.description.startsWith(namePrefix)) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'DELETE',
          url: `/apis/iam/v2/teams/${team.id}`,
          failOnStatusCode: false
        });
      }
    }
  });
});

Cypress.Commands.add('applyRulesAndWait', (attempts: number) => {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/apis/iam/v2/apply-rules'
  });

  waitUntilApplyRulesNotRunning(attempts);
});

Cypress.Commands.add('waitForNodemanagerNode', (nodeId: string, maxRetries: number) => {
  cy.request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'POST',
    url: '/api/v0/nodes/search',
    body: {
      order: 'DESC',
      page: 1,
      per_page: 100
    }
  })
  .then((resp: Cypress.ObjectLike) => {
    // to avoid getting stuck in an infinite loop
    expect(maxRetries).to.not.be.equal(0);
    if (resp.body.nodes && resp.body.nodes.length > 0 &&
      resp.body.nodes.some((node: any) => node.id === nodeId)) {
      return;
    }
    cy.wait(1000);
    cy.waitForNodemanagerNode(nodeId, maxRetries - 1);
  });
});

Cypress.Commands.add('waitForClientRunsNode', (nodeId: string, maxRetries: number) => {
  cy
  .request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'GET',
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${nodeId}`
  })
  .then((resp: Cypress.ObjectLike) => {
    // to avoid getting stuck in an infinite loop
    if (maxRetries === 0) {
      return;
    }
    if (resp.body && resp.body.length > 0 ) {
      return;
    }
    cy.wait(1000);
    cy.waitForClientRunsNode(nodeId, maxRetries - 1);
  });
});

Cypress.Commands.add('waitForComplianceNode', (nodeId: string, start: string, end: string,
  maxRetries: number) => {
  cy.request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'POST',
    url: '/api/v0/compliance/reporting/nodes/search',
    body: {
      filters: [
        { type: 'start_time', values: [start]},
        { type: 'end_time', values: [end]},
        { type: 'node_id', values: [nodeId]}
      ],
      order: 'DESC',
      page: 1,
      per_page: 100,
      sort: 'latest_report.end_time'
    }
  })
  .then((resp: Cypress.ObjectLike) => {
    // to avoid getting stuck in an infinite loop
    if (maxRetries === 0) {
      return;
    }
    if (resp.body.nodes && resp.body.nodes.length > 0 && resp.body.nodes[0].id === nodeId ) {
      return;
    }
    cy.wait(1000);
    cy.waitForComplianceNode(nodeId, start, end, maxRetries - 1);
  });
});

// helpers

function waitUntilApplyRulesNotRunning(attempts: number): void {
  if (attempts === -1) {
    throw new Error('apply-rules never finished');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: '/apis/iam/v2/apply-rules'
  }).then((response) => {
    if (response.body.state === 'not_running') {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for apply-rules to be not_running`);
      cy.wait(1000);
      waitUntilApplyRulesNotRunning(--attempts);
    }
  });
}

function LoginHelper(username: string) {
  cy.url().should('include', '/dex/auth/local');
  cy.server();
  // the gloabl permissions for the user that populates the initial permissions cache
  cy.route('GET', '/api/v0/auth/introspect').as('getAuthPopulateCache');

  // login
  cy.get('#login').type(username);
  cy.get('#password').type('chefautomate');

  cy.get('[type=submit]').click().then(() => {
    expect(localStorage.getItem('chef-automate-user')).to.contain(username);

    // close welcome modal if present
    cy.get('app-welcome-modal').invoke('hide');
    cy.saveStorage();

    cy.wait(['@getAuthPopulateCache']);
  });
}

function waitUntilAdminTokenPermissioned(attempts: number): void {
  for (let i = 0; i > attempts; i++) {
    cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: '/apis/iam/v2/projects',
    method: 'GET',
    failOnStatusCode: false
  }).then((response) => {
    if (response.status === 200) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for admin token to be permissioned`);
      cy.wait(1000);
      ++attempts;
    }
  });
  }
}

function cleanupProjectsByIDPrefixes(idPrefix: string): void {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: '/apis/iam/v2/projects'
  }).then((resp) => {
    const projectIds: string[] = resp.body.projects
      .filter((project: any) => project.id.startsWith(idPrefix))
      .map((project: any) => project.id);

    deleteProjects(projectIds, 0, false);
  });
}

function deleteProjects(projectIdsToDelete: string[], index: number,
  rulesWereDeleted: boolean): void {
  if (projectIdsToDelete.length === index) {
    if (rulesWereDeleted) {
      // Because rules were deleted we must first apply rules to be able to delete the project
      cy.applyRulesAndWait(100);
    }

    // Delete all the projects after all their rules are deleted
    for (const projectId of projectIdsToDelete) {
      deleteProject(projectId);
    }
  } else {
    // Delete all the projects rules
    const projectId = projectIdsToDelete[index];
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: `/apis/iam/v2/projects/${projectId}/rules`
    }).then((rulesResp) => {
      if (rulesResp.body.rules && rulesResp.body.rules.length > 0) {
        const finish = (): void => {
          deleteProjects(projectIdsToDelete, index + 1, true);
        };
        // Delete all the rules then call the deleteProjects with the next project
        deleteProjectRules(projectId, rulesResp.body.rules, finish);
      } else {
        deleteProjects(projectIdsToDelete, index + 1, rulesWereDeleted);
      }
    });
  }
}

// Delete one rule at a time and then when there are no rules left to delete
// call the finish function
function deleteProjectRules(projectId: string, rules: any[], finish: () => void) {
  if (rules.length === 0 ) {
    finish();
  } else {
    const [rule, ...rest] = rules;
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2/projects/${projectId}/rules/${rule.id}`
    }).then((deleteResp) => {
      expect(deleteResp.status).to.be.oneOf([200, 404]);
      deleteProjectRules(projectId, rest, finish);
    });
  }
}

function deleteProject(projectId: string) {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'DELETE',
    url: `/apis/iam/v2/projects/${projectId}`
  }).then((deleteResp) => {
    expect(deleteResp.status).to.be.oneOf([200, 404]);
  });
}

function cleanupV2IAMObjectByIDPrefix(idPrefix: string, iamObject: string): void {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: `/apis/iam/v2/${iamObject}`
  }).then((resp) => {
    for (const object of resp.body[iamObject]) {
      if (object.id.startsWith(idPrefix)) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'DELETE',
          url: `/apis/iam/v2/${iamObject}/${object.id}`
        }).then((deleteResp) => {
          expect(deleteResp.status).to.be.oneOf([200, 404]);
        });
      }
    }
  });
}
