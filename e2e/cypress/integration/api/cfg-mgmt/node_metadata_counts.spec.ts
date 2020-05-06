import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt node_metadata_counts', () => {
  const cypressPrefix = 'test-node_metadata_counts';
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

      cy.sendToDataCollector(node);
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

      cy.sendToDataCollector(node);
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

      cy.sendToDataCollector(node);
    });

    // Wait for nodes to be ingested
    cy.waitForClientRunsNode(clientRunsNodeId1);
    cy.waitForClientRunsNode(clientRunsNodeId2);
    cy.waitForClientRunsNode(clientRunsNodeId3);
  });

  after(() => {
    // delete all nodes created
    cy.deleteClientRunsNode(clientRunsNodeId1);
    cy.deleteClientRunsNode(clientRunsNodeId2);
    cy.deleteClientRunsNode(clientRunsNodeId3);
  });

  it('Get and test the node field value counts', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/api/v0/cfgmgmt/node_metadata_counts?type=platform&type=status'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.types.length).to.equal(2);
      const platformField = resp.body.types[0];
      expect(platformField.type).to.equal('platform');
      expect(platformField.values.length).to.equal(3);

      platformField.values.forEach( (valueCount: any) => {
        expect(valueCount.count).to.equal(1);
        expect(valueCount.value === 'macos' ||
        valueCount.value === 'windows' ||
        valueCount.value === 'linux').to.equal(true);
      });

      const statusField = resp.body.types[1];
      expect(statusField.type).to.equal('status');

      statusField.values.forEach( (valueCount: any) => {
        expect(valueCount.value === 'success' || valueCount.value === 'failure').to.equal(true);

        if ( valueCount.value === 'success' ) {
          expect(valueCount.count).to.equal(1);
        }
        if ( valueCount.value === 'failure' ) {
          expect(valueCount.count).to.equal(2);
        }
      });
    });
  });
});
