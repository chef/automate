import { uuidv4 } from '../../../support/helpers';

describe('Config-mgmt node_counts', () => {
  const cypressPrefix = 'test-node_counts';
  const clientRunsNodeId1 = uuidv4();
  const clientRunsNodeId2 = uuidv4();
  const clientRunsNodeId3 = uuidv4();
  const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
  before(() => {
    // Ingest 3 new nodes with increasing end_time
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment('2020-03-14T10:32:59Z');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId1;
      node.node_name = nodeName + '1';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.status = 'failure';
      node.node.chef_environment = cypressPrefix;

      cy.sendToDataCollector(node);
    });
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment('2020-03-15T10:32:59Z');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId2;
      node.node_name = nodeName + '2';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.status = 'success';
      node.node.chef_environment = cypressPrefix;

      cy.sendToDataCollector(node);
    });
    cy.fixture('converge/avengers1.json').then((node: any) => {
      const runEndDate = Cypress.moment('2020-03-16T10:32:59Z');
      const runId = uuidv4();
      node.entity_uuid = clientRunsNodeId3;
      node.node_name = nodeName + '3';
      node.id = runId;
      node.run_id = runId;
      node.start_time = runEndDate.subtract(5, 'minute').toISOString();
      node.end_time = runEndDate.toISOString();
      node.status = 'failure';
      node.node.chef_environment = cypressPrefix;

      cy.sendToDataCollector(node);
    });

    // Wait for nodes to be ingested
    cy.waitForClientRunsNode(clientRunsNodeId1);
    cy.waitForClientRunsNode(clientRunsNodeId2);
    cy.waitForClientRunsNode(clientRunsNodeId2);
  });

  after(() => {
    // delete all nodes created
    cy.deleteClientRunsNode(clientRunsNodeId1);
    cy.deleteClientRunsNode(clientRunsNodeId2);
    cy.deleteClientRunsNode(clientRunsNodeId3);
  });

  it('Get the node counts environment filter', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: `/api/v0/cfgmgmt/stats/node_counts?filter=environment:${cypressPrefix}`
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.total).to.equal(3);
      expect(resp.body.success).to.equal(1);
      expect(resp.body.failure).to.equal(2);
    });
  });

  it('Get the node counts date filters', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/api/v0/cfgmgmt/stats/node_counts?' +
        'start=2020-03-14T12%3A32%3A59Z&' +
        'end=2020-03-16T01%3A32%3A59Z&' +
        `filter=environment:${cypressPrefix}`
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.total).to.equal(1);
      expect(resp.body.success).to.equal(1);
      expect(resp.body.failure).to.equal(0);
    });
  });

  it('Get the node counts status filter', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/api/v0/cfgmgmt/stats/node_counts?' +
        'filter=status:failure&' +
        `filter=environment:${cypressPrefix}`
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.total).to.equal(2);
      expect(resp.body.success).to.equal(0);
      expect(resp.body.failure).to.equal(2);
    });
  });
});
