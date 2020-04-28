
import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt node runs daily time series statuses', () => {

  describe('two nodes 4 runs with two different environments', () => {
    const cypressPrefix = 'cfg-node-runs-time-series-statuses';
    const clientRunsNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    const runId1 =  uuidv4();
    const runId2 =  uuidv4();
    const runId3 =  uuidv4();
    const runId4 =  uuidv4();
    before(() => {
      // Add three CCRs a day apart with environment "forest"
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(12, 'hour');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId1;
        node.run_id = runId1;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(30, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId2;
        node.run_id = runId2;
        node.end_time = runEndDate.toISOString();
        node.status = 'success';
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(54, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId3;
        node.run_id = runId3;
        node.end_time = runEndDate.toISOString();
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // add a node within the three days with the environment "desert"
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(78, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId4;
        node.run_id = runId4;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();
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
      waitUntilRunIsIngested(10, clientRunsNodeId, runId4);
    });

    after(() => {
      // delete all nodes created
      deleteNode(10, clientRunsNodeId);
    });

    it('check statues', () => {

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url:
        `api/v0/cfgmgmt/node_runs_daily_status_time_series?node_id=${clientRunsNodeId}&days_ago=5`
      }).then((resp: Cypress.ObjectLike) => {
        expect(resp.body.durations.length).to.equal(5);
        expect(resp.body.durations[0].run_id).to.equal('');
        expect(resp.body.durations[0].status).to.equal('missing');

        expect(resp.body.durations[1].run_id).to.equal(runId4);
        expect(resp.body.durations[1].status).to.equal('failure');

        expect(resp.body.durations[2].run_id).to.equal(runId3);
        expect(resp.body.durations[2].status).to.equal('failure');

        expect(resp.body.durations[3].run_id).to.equal(runId2);
        expect(resp.body.durations[3].status).to.equal('success');

        expect(resp.body.durations[4].run_id).to.equal(runId1);
        expect(resp.body.durations[4].status).to.equal('failure');
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
