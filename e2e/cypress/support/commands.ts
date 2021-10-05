import { eventExist } from '../support/helpers';

// Cypress Commands: any action that could be taken in any test
// any command added in here must also have its signature added to index.d.ts


Cypress.Commands.add('sendToDataCollector', (report: any) => {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/data-collector/v0',
    body: report
  });
});

Cypress.Commands.add('login', (url: string, username: string) => {
  Cypress.log({
    displayName: 'login',
    message: [url]
  });
  // CYPRESS_BASE_URL environment variable must be set
  cy.visit(url);
  cy.reload();
  // only environments using SAML or LDAP present this login method selection
  cy.url().then(($url) => {
    if ($url.includes('/dex/auth?client_id=automate-session')) {
      cy.get('a').contains('Sign in as a local user').click().then(() => LoginHelper(username));
    } else  {
        LoginHelper(username);
      }
  });
});

Cypress.Commands.add('adminLogin', (url: string) => {
  Cypress.log({
    displayName: 'adminLogin',
    message: [url]
  });
  // CYPRESS_BASE_URL environment variable must be set
  return cy.login(url, 'admin');
});

Cypress.Commands.add('logout', () => {
  cy.get('[data-cy=user-profile-button]').click();
  cy.get('[data-cy=sign-out-button]').click();
  return cy.url().should('include', '/dex/auth');
});

// applyProjectsFilter will deselect all projects in the filter,
// check any projects by NAME (not id) passed -- if any -- and apply any changes.
// Preferring reliability over "natural" user behavior for this helper, we use forced clicks
// since the state of the dropdown may differ across the tests where it is called.
Cypress.Commands.add('applyProjectsFilter', (projectsToFilterOn: string[]) => {
  cy.get('app-projects-filter button#projects-filter-button').click();

  // deselect all projects
  // we add the force for when there are already no projects selected
  // and the Clear Selection button is hidden
  cy.get('chef-button#projects-filter-clear-selection').click({force : true});

  if (projectsToFilterOn.length > 0) {
    // check all desired checkboxes
    projectsToFilterOn.forEach(proj => {
      cy.get(`app-projects-filter chef-checkbox[title="${proj}"]`).click();
      cy.get(`app-projects-filter chef-checkbox[title="${proj}"]`)
        .should('have.attr', 'aria-checked', 'true');
    });
  }

  // apply projects filter
  cy.get('app-projects-filter chef-button#projects-filter-apply-changes')
    // we force the click here in case Apply button was disabled due to no net change
    // i.e. in the case that proj1, proj2 are selected,
    // selection is cleared,
    // and proj1, proj2 are selected again
    .click({force: true});
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

Cypress.Commands.add('cleanupIAMObjectsByIDPrefixes',
  (idPrefix: string, iamObjects: string[]) => {

  iamObjects.forEach((iamObject) => {
    if (iamObject === 'projects') {
      cleanupProjectsByIDPrefixes(idPrefix);
    } else {
      cleanupIAMObjectByIDPrefix(idPrefix, iamObject);
    }
  });
});

Cypress.Commands.add('applyRulesAndWait', () => {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/apis/iam/v2/apply-rules'
  });

  waitUntilApplyRulesNotRunning(100);
});

Cypress.Commands.add('waitForNodemanagerNode', (nodeId: string) => {
  waitForNodemanagerNodeLoop(nodeId, 10);
});

function waitForNodemanagerNodeLoop(nodeId: string, attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error('nodemanager node was never ingested');
  }
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
    if (resp.body.nodes && resp.body.nodes.length > 0 &&
      resp.body.nodes.some((node: any) => node.id === nodeId)) {
      return;
    }
    cy.wait(1000);
    waitForNodemanagerNodeLoop(nodeId, attemptsLeft - 1);
  });
}

Cypress.Commands.add('waitForClientRunsNode', (nodeId: string) => {
  waitForClientRunsNodeLoop(nodeId, 10);
});

function waitForClientRunsNodeLoop(nodeId: string, attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error('client runs node was never created');
  }
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
    if (resp.body.length === 1 && resp.body[0].id === nodeId) {
      return;
    }
    cy.wait(1000);
    waitForClientRunsNodeLoop(nodeId, attemptsLeft - 1);
  });
}

Cypress.Commands.add('waitForComplianceNode', (nodeId: string, start: string, end: string) => {
  waitForComplianceNodeLoop(nodeId, start, end, 100);
});

function waitForComplianceNodeLoop(nodeId: string, start: string, end: string,
  attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error(`Compliance node with ID ${nodeId} was not created`);
  }
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
    if (resp.body.nodes && resp.body.nodes.length > 0 && resp.body.nodes[0].id === nodeId ) {
      return;
    }
    cy.wait(1000);
    waitForComplianceNodeLoop(nodeId, start, end, attemptsLeft - 1);
  });
}

Cypress.Commands.add('waitForAction', (entityName: string, start: string, end: string) => {
  waitForActionLoop(entityName, start, end, 30);
});

function waitForActionLoop(entityName: string, start: string, end: string,
  attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error(`Action with entity name ${entityName} was not created`);
  }
  cy.request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'GET',
    url: `api/v0/eventfeed?collapse=false&page_size=100&start=${start}&end=${end}`
  }).then((resp: Cypress.ObjectLike) => {
      if (resp.body.events && resp.body.events.length > 0 &&
        eventExist(entityName, resp.body.events)) {
        return;
      }
      cy.wait(1000);
      waitForActionLoop(entityName, start, end, attemptsLeft - 1);
  });
}

Cypress.Commands.add('deleteClientRunsNode', (clientRunsNodeId: string, attempts: number) => {
  deleteClientRunsNodeLoop(clientRunsNodeId, 10);
});

function deleteClientRunsNodeLoop(clientRunsNodeId: string,
  attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error('client runs node was never deleted');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 0) {
      return;
    } else {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/node-multiple-deletes',
        body: {
          node_ids: [
            clientRunsNodeId
          ]
        },
        failOnStatusCode: true
      });
      cy.log(`${attemptsLeft} attempts remaining: waiting for node` +
        ` ${clientRunsNodeId} to be deleted`);
      cy.wait(1000);
      deleteClientRunsNodeLoop(clientRunsNodeId, attemptsLeft - 1);
    }
  });
}

// This function is only waiting for the node to be deleted. It is not deleting it.
Cypress.Commands.add('waitUntilNodemanagerNodeIsDeleted', (nodeName: string) => {
  waitUntilNodemanagerNodeIsDeletedLoop(nodeName, 10);
});

function waitUntilNodemanagerNodeIsDeletedLoop(nodeName: string,
  attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error(`nodemanager node with name ${nodeName} was never deleted`);
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/api/v0/nodes/search',
    body: {
      filters: [
        {key: 'manager_id', values: ['']},
        {key: 'name', 'values': [nodeName]}
      ]
    }
  }).then((response) => {
    if (response.body.nodes.length === 0) {
      return;
    } else {
      cy.log(`${attemptsLeft} attempts remaining: waiting for nodemanager node to be deleted`);
      cy.wait(1000);
      waitUntilNodemanagerNodeIsDeletedLoop(nodeName, attemptsLeft - 1);
    }
  });
}

Cypress.Commands.add('waitUntilConfigMgmtNodeIsDeleted', (clientRunsNodeId: string) => {
  waitUntilConfigMgmtNodeIsDeletedLoop(clientRunsNodeId, 10);
});

function waitUntilConfigMgmtNodeIsDeletedLoop(clientRunsNodeId: string, attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error(`infra node with ID ${clientRunsNodeId} was not deleted`);
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 0) {
      return;
    } else {
      cy.log(`${attemptsLeft} attempts remaining: waiting for node` +
        ` ${clientRunsNodeId} to be deleted`);
      cy.wait(1000);
      waitUntilConfigMgmtNodeIsDeletedLoop(clientRunsNodeId, attemptsLeft - 1);
    }
  });
}

Cypress.Commands.add('waitUntilRunIsIngested', (clientRunsNodeId: string, runId: string) => {
  waitUntilRunIsIngestedLoop(clientRunsNodeId, runId, 10);
});

function waitUntilRunIsIngestedLoop(clientRunsNodeId: string,
  runId: string, attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error(`run with ID ${clientRunsNodeId} was never ingested`);
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes/${clientRunsNodeId}/runs/${runId}`,
    failOnStatusCode: false
  }).then((response) => {
    if (response.status !== 404 && response.body.id === runId) {
      return;
    } else {
      cy.log(`${attemptsLeft} attempts remaining: waiting for run ${runId} to be ingested`);
      cy.wait(1000);
      waitUntilRunIsIngestedLoop(clientRunsNodeId, runId, attemptsLeft - 1);
    }
  });
}

Cypress.Commands.add('waitUntilNodeIsMissing', (clientRunsNodeId: string) => {
  waitUntilNodeIsMissingLoop(clientRunsNodeId, 10);
});

function waitUntilNodeIsMissingLoop(clientRunsNodeId: string, attemptsLeft: number) {
  if (attemptsLeft === -1) {
    throw new Error('infra node was never marked missing');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 1 && response.body[0].id === clientRunsNodeId &&
      response.body[0].status === 'missing') {
      return;
    } else {
      cy.log(
        `${attemptsLeft} attempts remaining: waiting for node ${clientRunsNodeId}` +
        'to have status missing');
      cy.wait(1000);
      waitUntilNodeIsMissingLoop(clientRunsNodeId, attemptsLeft - 1);
    }
  });
}

// the helpers below are used only in the Cypress commands defined in this file
// other helpers for use in tests can be found in helpers.ts
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
  if (Cypress.$('app-welcome-modal').length) {  // zero length means not found
    cy.get('[data-cy=close-x]').click();
  }
  cy.url().should('include', '/dex/auth/local');
  cy.server();
  // the global permissions for the user that populates the initial permissions cache
  cy.route('GET', '/apis/iam/v2/introspect').as('getAuthPopulateCache');

  // login
  cy.get('#login').type(username);
  cy.get('#password').type('chefautomate');

  cy.get('[type=submit]').click().then(() => {

    // close welcome modal if present
    cy.get('[data-cy=close-x]').click();
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
      cy.applyRulesAndWait();
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
        // make sure there are no rules staged for deletion that need to be applied
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${projectId}`
        }).then((projectResp) => {
          if (projectResp.body.project.status === 'EDITS_PENDING') {
            rulesWereDeleted = true;
          }
        });

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

function cleanupIAMObjectByIDPrefix(idPrefix: string, iamObject: string): void {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'GET',
    url: `/apis/iam/v2/${iamObject}`
  }).then((resp) => {
    for (const object of resp.body[iamObject]) {
      console.log(object);
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
