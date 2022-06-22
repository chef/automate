import { uuidv4 } from '../../../support/helpers';

describe('delete missing node from UI', () => {
  const cypressPrefix = 'ui-delete-missing-node';
  const clientRunsNodeId = uuidv4();
  const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;

  before(() => {
    // Add node that ran a month ago
    cy.fixture('converge/avengers1.json').then((node) => {
      const runEndDate = Cypress.moment().subtract(1, 'month');
      node.entity_uuid = clientRunsNodeId;
      node.node_name = nodeName;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      cy.sendToDataCollector(node);
    });

    cy.waitForClientRunsNode(clientRunsNodeId);

    // Update the mark nodes missing job to mark the nodes missing
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

    // wait for nodes to be marked missing
    cy.waitUntilNodeIsMissing(clientRunsNodeId);
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
  });

  it('from client runs page delete nodes', () => {
    if (Cypress.$('app-welcome-modal').length) {  // zero length means not found
      cy.get('app-welcome-modal').invoke('hide');
    }

    cy.adminLogin('/infrastructure/client-runs');

    // Check the check box to delete all missing nodes
    cy.get('chef-checkbox.header').click();


    // Click the delete all button
    cy.get('chef-button.delete-button').click();

    // Click the confim delete button
    cy.get('chef-modal chef-button.delete-button-confirm').click();

    // wait until all nodes are delete API check
    cy.waitUntilConfigMgmtNodeIsDeleted(clientRunsNodeId);
  });
});
