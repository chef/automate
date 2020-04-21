import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt nodes_field_value_counts', () => {
  const cypressPrefix = 'test-nodes_field_value_counts';
  const clientRunsNodeId1 = uuidv4();
  const clientRunsNodeId2 = uuidv4();
  const clientRunsNodeId3 = uuidv4();
  const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
  before(() => {
    // Add three nodes
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment().subtract(12, 'hour');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId1;
      node.node_name = nodeName + '1';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.node.automatic.platform = 'windows';
      node.status = 'failure';
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment().subtract(12, 'hour');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId2;
      node.node_name = nodeName + '2';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.node.automatic.platform = 'linux';
      node.status = 'success';
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment().subtract(12, 'hour');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId3;
      node.node_name = nodeName + '3';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.node.automatic.platform = 'macos';
      node.status = 'failure';
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });

    // Wait for nodes to be ingested
    waitUntilNodeIsIngested(10, clientRunsNodeId1);
    waitUntilNodeIsIngested(10, clientRunsNodeId2);
    waitUntilNodeIsIngested(10, clientRunsNodeId3);
  });

  after(() => {
    // delete all nodes created
    deleteNode(10, clientRunsNodeId1);
    deleteNode(10, clientRunsNodeId2);
    deleteNode(10, clientRunsNodeId3);
  });

  it('Get and test the node field value counts', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/api/v0/cfgmgmt/nodes_field_value_counts?terms=platform&terms=status'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.fields.length).to.equal(2);
      const platformField = resp.body.fields[0];
      expect(platformField.field).to.equal('platform');
      expect(platformField.terms.length).to.equal(3);

      platformField.terms.forEach( (term: any) => {
        expect(term.count).to.equal(1);
        expect(term.term === 'macos' ||
        term.term === 'windows' ||
        term.term === 'linux').to.equal(true);
      });

      const statusField = resp.body.fields[1];
      expect(statusField.field).to.equal('status');

      statusField.terms.forEach( (term: any) => {
        expect(term.term === 'success' || term.term === 'failure').to.equal(true);

        if ( term.term === 'success' ) {
          expect(term.count).to.equal(1);
        }
        if ( term.term === 'failure' ) {
          expect(term.count).to.equal(2);
        }
      });
    });
  });
});

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
  }).then((response: any) => {
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
