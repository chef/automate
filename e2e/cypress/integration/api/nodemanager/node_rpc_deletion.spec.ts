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
      waitUntilConfigMgmtNodeIsIngested(10, clientRunsNodeId1);
      waitUntilConfigMgmtNodeIsIngested(10, clientRunsNodeId2);

      // Wait for the node to appear in the node manager
      waitUntilNodemanagerNodeIsIngested(10, nodeName1);
      waitUntilNodemanagerNodeIsIngested(10, nodeName2);

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
      waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId1);
      waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId2);
    });

    it('nodemanager nodes are deleted', () => {
      waitUntilNodemanagerNodeIsDeleted(10, nodeName1);
      waitUntilNodemanagerNodeIsDeleted(10, nodeName2);
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
      waitUntilConfigMgmtNodeIsIngested(10, clientRunsNodeId);

      // Wait for the node to appear in the node manager
      waitUntilNodemanagerNodeIsIngested(10, nodeName);

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
      waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
    });

    it('nodemanager node is deleted', () => {
      waitUntilNodemanagerNodeIsDeleted(10, nodeName);
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
      waitUntilConfigMgmtNodeIsIngested(10, clientRunsNodeId);

      // Wait for the node to appear in the node manager
      waitUntilNodemanagerNodeIsIngested(10, nodeName);

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
      waitUntilConfigMgmtNodeIsDeleted(10, clientRunsNodeId);
    });

    it('nodemanager node is deleted', () => {
      waitUntilNodemanagerNodeIsDeleted(10, nodeName);
    });
  });
});

function waitUntilNodemanagerNodeIsDeleted(attempts: number, nodeName: string): void {
  if (attempts === -1) {
    throw new Error('nodemanager node was never deleted');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/api/v0/nodes/search',
    body: {
      filters: [
        {key: 'manager_id', values: ['']},
        {key: 'name', 'values': [nodeName]}
      ]
    }
  }).then((response) => {
    if (response.body.nodes.length === 0) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for nodemanager node to be deleted`);
      cy.wait(1000);
      waitUntilNodemanagerNodeIsDeleted(--attempts, nodeName);
    }
  });
}

function waitUntilNodemanagerNodeIsIngested(attempts: number, nodeName: string): void {
  if (attempts === -1) {
    throw new Error('nodemanager node was never ingested');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/api/v0/nodes/search',
    body: {
      filters: [
        {key: 'manager_id', values: ['']},
        {key: 'name', 'values': [nodeName]}
      ]
    }
  }).then((response) => {
    if (response.body.nodes.length === 1) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node to be missing`);
      cy.wait(1000);
      waitUntilNodemanagerNodeIsIngested(--attempts, nodeName);
    }
  });
}

function waitUntilConfigMgmtNodeIsIngested(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('config mgmt node was never ingested');
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
      waitUntilConfigMgmtNodeIsIngested(--attempts, clientRunsNodeId);
    }
  });
}

function waitUntilConfigMgmtNodeIsDeleted(attempts: number, clientRunsNodeId: string): void {
  if (attempts === -1) {
    throw new Error('config mgmt node was not deleted');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
  }).then((response) => {
    if (response.body.length === 0) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for node ${clientRunsNodeId} to be deleted`);
      cy.wait(1000);
      waitUntilConfigMgmtNodeIsDeleted(--attempts, clientRunsNodeId);
    }
  });
}

