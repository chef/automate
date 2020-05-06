import { uuidv4 } from '../../../support/helpers';

describe('Nodemanager config mgmt node rpc deletion', () => {
  describe('multiple node deletion', () => {
    const cypressPrefix = 'test-nodemanager-node-rpc-delete-nonid';
    const clientRunsNodeId1 = uuidv4();
    const nodeName1 = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}-1`;
    const clientRunsNodeId2 = uuidv4();
    const nodeName2 = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}-2`;
    before(() => {
      // Create the first node
      cy.fixture('converge/avengers1.json').then((node) => {
        const runId =  uuidv4();
        node.entity_uuid = clientRunsNodeId1;
        node.node_name = nodeName1;
        node.id = runId;
        node.run_id = runId;
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // Create the second node
      cy.fixture('converge/avengers1.json').then((node) => {
        const runId =  uuidv4();
        node.entity_uuid = clientRunsNodeId2;
        node.node_name = nodeName2;
        node.id = runId;
        node.run_id = runId;
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // Wait for that config mgmt node to appear
      cy.waitForClientRunsNode(clientRunsNodeId1, 10);
      cy.waitForClientRunsNode(clientRunsNodeId2, 10);

      // Wait for the node to appear in the node manager
      cy.waitForNodemanagerNode(clientRunsNodeId1, 10);
      cy.waitForNodemanagerNode(clientRunsNodeId2, 10);

      // delete both nodes
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/node-multiple-deletes',
        body: {
          node_ids: [clientRunsNodeId1, clientRunsNodeId2]
        }
      });

      // Wait for the node to be removed from config mgmt
      cy.waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId1);
      cy.waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId2);
    });

    it('nodemanager nodes are deleted', () => {
      cy.waitUntilNodemanagerNodeIsDeleted(10, nodeName1);
      cy.waitUntilNodemanagerNodeIsDeleted(10, nodeName2);
    });
  });

  describe('delete with org, chef server, and node name', () => {
    const cypressPrefix = 'test-nodemanager-node-rpc-delete-nonid';
    const clientRunsNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    const org = 'NOAA';
    const chefServer = 'chef_server_fqdn';
    before(() => {
      // Create a node
      cy.fixture('converge/avengers1.json').then((node) => {
        node.entity_uuid = clientRunsNodeId;
        node.node_name = nodeName;
        node.organization_name = org;
        node.chef_server_fqdn = chefServer;
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

      // delete the node
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/nodedelete',
        body: {
          node_name: nodeName,
          organization_name: org,
          service_hostname: chefServer
        }
      });

      // Wait for the node to be removed from config mgmt
      cy.waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
    });

    it('nodemanager node is deleted', () => {
      cy.waitUntilNodemanagerNodeIsDeleted(10, nodeName);
    });
  });

  describe('delete with id', () => {
    const cypressPrefix = 'test-nodemanager-node-rpc-delete-id';
    const clientRunsNodeId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}`;
    before(() => {
      // Create a node
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

      // delete the node
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/nodedelete',
        body: {
          node_id: clientRunsNodeId
        }
      });

      // Wait for the node to be removed from config mgmt
      cy.waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
    });

    it('nodemanager node is deleted', () => {
      cy.waitUntilNodemanagerNodeIsDeleted(10, nodeName);
    });
  });
});
