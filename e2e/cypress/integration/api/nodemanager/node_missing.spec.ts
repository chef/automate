import { uuidv4 } from '../../../support/helpers';

describe('Nodemanager node missing', () => {

  describe('Normal ingest marking node missing', () => {
    const cypressPrefix = 'test-nodemanager-node-missing-normal';
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

      // Wait for that node to appear
      waitUntilNodeIsIngested(10, clientRunsNodeId);

      // Set the ConfigureNodesMissingScheduler threshold to zero
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
    });

    after(() => {
      // delete node
      deleteNode(10, clientRunsNodeId);

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
    });

    it('nodemanager nodes state is missing', () => {
      // Check that the node is missing from the nodemanager
      waitUntilNodemanagerNodeState(10, nodeName, 'MISSING');
    });
  });

  describe('missing to non missing new report', () => {
    const cypressPrefix = 'test-nodemanager-node-missing-nonmissing-report';
    const clientRunsNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    before(() => {
      // Send a really old CCR
      cy.fixture('converge/avengers1.json').then((node) => {
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.run_id = uuidv4();
        node.id = node.run_id;
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // Wait for that node to appear
      waitUntilNodeIsIngested(10, clientRunsNodeId);

      // Force a node missing check
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/retention/nodes/missing-nodes/config',
        body: {
          threshold: '48h',
          running: true,
          every: '15m'
        }
      });

      // Wait until the node is missing
      waitUntilNodeIsMissing(10, clientRunsNodeId);

      // Send a CCR within the last day
      cy.fixture('converge/avengers1.json').then((node) => {
        const runEndDate = Cypress.moment();
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.run_id = uuidv4();
        node.id = node.run_id;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });
    });

    after(() => {
      // delete node
      deleteNode(10, clientRunsNodeId);

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
    });

    it('nodemanager node state is running', () => {
      // Check that the nodemanager's node was changed to running
      waitUntilNodemanagerNodeState(10, nodeName, 'RUNNING');
    });
  });
});

function waitUntilNodemanagerNodeState(attempts: number, nodeName: string, state: string): void {
  if (attempts === -1) {
    throw new Error('node was never marked missing');
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
    if (response.body.nodes.length === 1 && response.body.nodes[0].name === nodeName &&
      response.body.nodes[0].state === state) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node to be missing`);
      cy.wait(1000);
      waitUntilNodemanagerNodeState(--attempts, nodeName, state);
    }
  });
}

function waitUntilNodeIsIngested(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('node was never ingested');
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
      waitUntilNodeIsIngested(--attempts, clientRunsNodeId);
    }
  });
}

function deleteNode(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('node was never deleted');
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
      cy.log(`${attempts} attempts remaining: waiting for node ${clientRunsNodeId} to be deleted`);
      cy.wait(1000);
      deleteNode(--attempts, clientRunsNodeId);
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
