import { uuidv4 } from '../../../support/helpers';

describe('Nodemanager config mgmt node deletion', () => {
  const cypressPrefix = 'test-nodemanager-node-lifecycle-delete';
  const clientRunsNodeId = uuidv4();
  const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
  before(() => {
    // Send a really old CCR
    cy.fixture('converge/avengers1.json').then((node) => {
      node.entity_uuid = clientRunsNodeId;
      node.node_name = nodeName;
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });

    // Wait for that config mgmt node to appear
    waitUntilConfigMgmtNodeIsIngested(10, clientRunsNodeId);

    // Wait for the node to appear in the node manager
    waitUntilNodemanagerNodeIsIngested(10, nodeName);

    // Set the ConfigureNodesMissingScheduler threshold to 1 hour
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/retention/nodes/missing-nodes/config',
      body: {
        threshold: '1h',
        running: true,
        every: '15m'
      }
    });

    // Wait until the node is missing
    waitUntilNodeIsMissing(10, clientRunsNodeId);

    // Set the ConfigureNodesDeleteScheduler threshold to 0 minutes
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/retention/nodes/missing-nodes-deletion/config',
      body: {
        threshold: '1s',
        running: true,
        every: '15m'
      }
    });

    // Wait until the config mgmt node is deleted
    waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
  });

  after(() => {
    // Set the ConfigureNodesMissingScheduler threshold back
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/retention/nodes/missing-nodes/config',
      body: {
        threshold: '24h',
        running: true,
        every: '15m'
      }
    });

    // Set the ConfigureNodesDeleteScheduler threshold to 0 minutes
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/retention/nodes/missing-nodes-deletion/config',
      body: {
        threshold: '30d',
        running: true,
        every: '15m'
      }
    });
  });

  it('nodemanager nodes is deleted', () => {
    waitUntilNodemanagerNodeIsDeleted(10, nodeName);
  });
});

function waitUntilNodemanagerNodeIsDeleted(attempts: number, nodeName: string): void {
  if (attempts === -1) {
    throw new Error('nodemanager node was never deleted');
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
      cy.log(`${attempts} attempts remaining: waiting for nodemanager node to be deleted`);
      cy.wait(1000);
      waitUntilNodemanagerNodeIsDeleted(--attempts, nodeName);
    }
  });
}

function waitUntilNodemanagerNodeIsIngested(attempts: number, nodeName: string): void {
  if (attempts === -1) {
    throw new Error('nodemanager node was never ingested');
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
    if (response.body.nodes.length === 1) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node to be missing`);
      cy.wait(1000);
      waitUntilNodemanagerNodeIsIngested(--attempts, nodeName);
    }
  });
}

function waitUntilConfigMgmtNodeIsIngested(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('config mgmt node was never ingested');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 1 && response.body[0].id === clientRunsNodeId) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node ${clientRunsNodeId} to be ingested`);
      cy.wait(1000);
      waitUntilConfigMgmtNodeIsIngested(--attempts, clientRunsNodeId);
    }
  });
}

function waitUntilConfigMgmtNodeIsDeleted(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('config mgmt node was not deleted');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 0) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node ${clientRunsNodeId} to be deleted`);
      cy.wait(1000);
      waitUntilConfigMgmtNodeIsDeleted(--attempts, clientRunsNodeId);
    }
  });
}

function waitUntilNodeIsMissing(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('node was never marked missing');
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
        `${attempts} attempts remaining: waiting for node ${clientRunsNodeId}` +
        'to have status missing');
      cy.wait(1000);
      waitUntilNodeIsMissing(--attempts, clientRunsNodeId);
    }
  });
}

