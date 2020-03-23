
import { describeIfIAMV2p1 } from '../../../support/constants';
import { uuidv4 } from '../../../support/helpers';

describeIfIAMV2p1('Config-mgmt check-in time series', () => {

  describe('two nodes 4 runs with two different environments', () => {
    const cypressPrefix = 'test-cfg-check-in';
    const clientRunsNodeId = uuidv4();
    const clientRunsFilteredOutNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    before(() => {

      // Add three CCRs a day apart with environment "forest"
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runId =  uuidv4();
        const runEndDate = Cypress.moment().subtract(65, 'minute');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId;
        node.run_id = runId;
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
        const runId =  uuidv4();
        const runEndDate = Cypress.moment().subtract(27, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId;
        node.run_id = runId;
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
        const runId =  uuidv4();
        const runEndDate = Cypress.moment().subtract(50, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId;
        node.run_id = runId;
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
        const runId =  uuidv4();
        const runEndDate = Cypress.moment().subtract(75, 'minute');
        node.entity_uuid = clientRunsFilteredOutNodeId;
        node.node_name = nodeName;
        node.id = runId;
        node.run_id = runId;
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

      waitUntilNodeIsIngested(10, clientRunsFilteredOutNodeId);
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
