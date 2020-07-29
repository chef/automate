import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt missing node duration counts', () => {

  describe('add one node and ensure it is counted', () => {
    const cypressPrefix = 'test-cfg-missing-node-duration-counts';
    const clientRunsNodeId = uuidv4();
    const clientRunsFilteredOutNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    before(() => {
      // Add a node
      cy.fixture('converge/avengers1.json').then((node: any) => {
        const runEndDate = Cypress.moment().subtract(12, 'hour');
        const runId = uuidv4();
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.id = runId;
        node.run_id = runId;
        node.start_time = runEndDate.subtract(5, 'minute').toISOString();
        node.end_time = runEndDate.toISOString();

        cy.sendToDataCollector(node);
      });

      // Wait for it to be ingested
      cy.waitForClientRunsNode(clientRunsNodeId);
    });

    after(() => {
      // delete all nodes created
      cy.deleteClientRunsNode(clientRunsNodeId);
    });

    it('Ensure the node is counted in the missing node duration counts', () => {
      // Get missing node duration counts from now

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: 'api/v0/cfgmgmt/stats/missing_node_duration_counts?durations=0d'
      }).then((resp: Cypress.ObjectLike) => {
        // ensure there is one counted
        expect(resp.body.counted_durations[0].count).to.equal(1);
        expect(resp.body.counted_durations[0].duration).to.equal('0d');
      });
    });
  });
});
