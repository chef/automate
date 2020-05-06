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

        cy.sendToDataCollector(node);
      });

      // Wait for that node to appear
      cy.waitForClientRunsNode(clientRunsNodeId);

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
      cy.waitUntilNodeIsMissing(clientRunsNodeId);
    });

    after(() => {
      // delete node
      cy.deleteClientRunsNode(clientRunsNodeId);

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
      waitUntilNodemanagerNodeState(nodeName, 'MISSING');
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

        cy.sendToDataCollector(node);
      });

      // Wait for that node to appear
      cy.waitForClientRunsNode(clientRunsNodeId);

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
      cy.waitUntilNodeIsMissing(clientRunsNodeId);

      // Send a CCR within the last day
      cy.fixture('converge/avengers1.json').then((node) => {
        const runEndDate = Cypress.moment();
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.run_id = uuidv4();
        node.id = node.run_id;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();

        cy.sendToDataCollector(node);
      });
    });

    after(() => {
      // delete node
      cy.deleteClientRunsNode(clientRunsNodeId);

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
      waitUntilNodemanagerNodeState(nodeName, 'RUNNING');
    });
  });
});

function waitUntilNodemanagerNodeState(nodeName: string, state: string) {
  waitUntilNodemanagerNodeStateLoop(nodeName, state, 10);
}

function waitUntilNodemanagerNodeStateLoop(nodeName: string, state: string,
  attemptsLeft: number): void {
  if (attemptsLeft === -1) {
    throw new Error(`nodemanager node with name ${nodeName} was never marked missing`);
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
      cy.log(`${attemptsLeft} attempts remaining: waiting for node to be missing`);
      cy.wait(1000);
      waitUntilNodemanagerNodeStateLoop(nodeName, state, attemptsLeft - 1);
    }
  });
}
