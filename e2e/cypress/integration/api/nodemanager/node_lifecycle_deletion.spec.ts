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
    cy.waitForClientRunsNode(clientRunsNodeId, 10);

    // Wait for the node to appear in the node manager
    cy.waitForNodemanagerNode(clientRunsNodeId, 10);

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
    cy.waitUntilNodeIsMissing(10, clientRunsNodeId);

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
    cy.waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
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
    cy.waitUntilNodemanagerNodeIsDeleted(10, nodeName);
  });
});

