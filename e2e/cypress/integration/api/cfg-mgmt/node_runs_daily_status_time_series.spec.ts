
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

        cy.sendToDataCollector(node);
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

        cy.sendToDataCollector(node);
      });

      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(54, 'hours');
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId3;
        node.run_id = runId3;
        node.end_time = runEndDate.toISOString();
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();

        cy.sendToDataCollector(node);
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

        cy.sendToDataCollector(node);
      });

      cy.waitForClientRunsNode(clientRunsNodeId);
      cy.waitUntilRunIsIngested(clientRunsNodeId, runId1);
      cy.waitUntilRunIsIngested(clientRunsNodeId, runId2);
      cy.waitUntilRunIsIngested(clientRunsNodeId, runId3);
      cy.waitUntilRunIsIngested(clientRunsNodeId, runId4);
    });

    after(() => {
      // delete all nodes created
      cy.deleteClientRunsNode(clientRunsNodeId);
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
