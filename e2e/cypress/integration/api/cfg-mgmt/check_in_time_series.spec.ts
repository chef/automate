
import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt check-in time series', () => {

  describe('two nodes 4 runs with two different environments', () => {
    const cypressPrefix = 'test-cfg-check-in';
    const clientRunsNodeId = uuidv4();
    const clientRunsFilteredOutNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    before(() => {
      const runId1 =  uuidv4();
      const runId2 =  uuidv4();
      const runId3 =  uuidv4();
      const runId4 =  uuidv4();

      // Add three CCRs a day apart with environment "forest"
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(65, 'minute');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId1;
        node.run_id = runId1;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();
        node.node.chef_environment = 'forest';
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(27, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId2;
        node.run_id = runId2;
        node.end_time = runEndDate.toISOString();
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.node.chef_environment = 'forest';
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(50, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId3;
        node.run_id = runId3;
        node.end_time = runEndDate.toISOString();
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.node.chef_environment = 'forest';
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // add a node within the three days with the environment "desert"
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(75, 'minute');
        node.entity_uuid = clientRunsFilteredOutNodeId;
        node.node_name = nodeName;
        node.id = runId4;
        node.run_id = runId4;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();
        node.node.chef_environment = 'desert';
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      waitUntilNodeIsIngested(10, clientRunsNodeId);
      waitUntilRunIsIngested(10, clientRunsNodeId, runId1);
      waitUntilRunIsIngested(10, clientRunsNodeId, runId2);
      waitUntilRunIsIngested(10, clientRunsNodeId, runId3);

      waitUntilNodeIsIngested(10, clientRunsFilteredOutNodeId);
      waitUntilRunIsIngested(10, clientRunsFilteredOutNodeId, runId4);
    });

    after(() => {
      // delete all nodes created
      deleteNode(10, clientRunsNodeId);
      deleteNode(10, clientRunsFilteredOutNodeId);
    });

    it('filter for the forest environment', () => {
      // request the check-in time series for the past three days with an
      // organization_name = "forest" filter
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: 'api/v0/cfgmgmt/stats/checkin_counts_timeseries?days_ago=3&filter=environment:forest'
      }).then((resp: Cypress.ObjectLike) => {
        // ensure all three buckets have count one
        expect(resp.body.counts.length).to.equal(3);
        expect(resp.body.counts[0].count).to.equal(1);
        expect(resp.body.counts[1].count).to.equal(1);
        expect(resp.body.counts[2].count).to.equal(1);
        expect(resp.body.counts[2].total).to.equal(1);
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: 'api/v0/cfgmgmt/stats/checkin_counts_timeseries?days_ago=3'
      }).then((resp: Cypress.ObjectLike) => {
        expect(resp.body.counts.length).to.equal(3);
        expect(resp.body.counts[0].count).to.equal(1);
        expect(resp.body.counts[1].count).to.equal(1);
        expect(resp.body.counts[2].count).to.equal(2); // include the filtered out node
        expect(resp.body.counts[2].total).to.equal(2);
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

function waitUntilRunIsIngested(attempts: number, clientRunsNodeId: string,
  runId: string): void {
  if (attempts === -1) {
    throw new Error('run was never ingested');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes/${clientRunsNodeId}/runs/${runId}`,
    failOnStatusCode: false
  }).then((response) => {
    if (response.status !== 404 && response.body.id === runId) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for run ${runId} to be ingested`);
      cy.wait(1000);
      waitUntilRunIsIngested(--attempts, clientRunsNodeId, runId);
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
