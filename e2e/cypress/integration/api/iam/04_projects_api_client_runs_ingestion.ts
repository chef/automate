import { describeIfIAMV2p1 } from '../../constants';

describeIfIAMV2p1('Client Runs Ingestion project tagging', () => {
  const cypressPrefix = 'test-projects-api-client-runs';
  const project9 = {
    id: `${cypressPrefix}-project9-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'project 9'
  };

  before(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    // create the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2beta/projects',
      body: project9
    });
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  for (const [attribute, value] of [
    ['CHEF_ORGANIZATION', '75th Rangers'],
    ['CHEF_SERVER', 'example.org'],
    ['ENVIRONMENT', 'arctic'],
    ['CHEF_POLICY_GROUP', 'red_ring'],
    ['CHEF_POLICY_NAME', 'fire'],
    ['CHEF_ROLE', 'backend'],
    ['CHEF_TAG', 'v3']
  ]) {
    it(`attribute ${attribute} gets tagged on node`, () => {
      const nodeId = uuidv4();

      // Create rule for attribute ${attribute}
      const rule = {
        id: 'rule-' + attribute.toLowerCase(),
        name: 'rule ' + attribute,
        type: 'NODE',
        project_id: project9.id,
        conditions: [
          {
            attribute: attribute,
            operator: 'EQUALS',
            values: [value]
          }
        ]
      };

      // create rule with attribute value
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2beta/projects/${rule.project_id}/rules`,
        body: rule
      });

      // apply rules
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2beta/apply-rules'
      });

      waitUntilApplyRulesNotRunning(100);

      // Ensure there are no nodes matching this project
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: project9.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0);
      });

      // Ingest node with that same attribue
      cy.fixture('converge/avengers1.json').then((node) => {
        setAttributeValue(node, attribute, value);
        node.entity_uuid = nodeId;
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/data-collector/v0',
          body: node
        });
      });

      // wait for the node to be ingested
      waitForNodes(project9.id, 30);

      // Ensure the node is tagged with the correct project
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: project9.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(1);
        expect(response.body[0].id).to.equal(nodeId);
      });

      // Delete node
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/node-multiple-deletes',
        body: {
          node_ids: [nodeId]
        },
        failOnStatusCode: false
      });

      // delete rule
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2beta/projects/${rule.project_id}/rules/${rule.id}`
      }).then((response) => {
        expect(response.status).to.equal(200);
      });

      // apply rules to reset node to unassigned
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2beta/apply-rules'
      }).then((response) => {
        expect(response.status).to.equal(200);
      });
      waitUntilApplyRulesNotRunning(100);
    });
  }
});

function setAttributeValue(node: any, attributeType: string, value: string) {
  switch (attributeType) {
    case 'CHEF_ORGANIZATION': {
      node.organization_name = value;
      break;
    }
    case 'CHEF_SERVER': {
      node.chef_server_fqdn = value;
      break;
    }
    case 'ENVIRONMENT': {
      node.node.chef_environment = value;
      break;
    }
    case 'CHEF_POLICY_GROUP': {
      node.policy_group = value;
      break;
    }
    case 'CHEF_POLICY_NAME': {
      node.policy_name = value;
      break;
    }
    case 'CHEF_ROLE': {
      node.node.automatic.roles = [value];
      break;
    }
    case 'CHEF_TAG': {
      node.node.normal.tags = [value];
      break;
    }
    default: {
      expect(0).to.equal(1);
      break;
    }
  }
}

function waitUntilApplyRulesNotRunning(attempts: number): void {
  if (attempts === -1) {
    throw new Error('apply-rules never finished');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: '/apis/iam/v2beta/apply-rules'
  }).then((response) => {
    if (response.body.state === 'not_running') {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for apply-rules to be not_running`);
      cy.wait(1000);
      waitUntilApplyRulesNotRunning(--attempts);
    }
  });
}

function waitForNodes(project: string, maxRetries: number) {
  cy
    .request({
      headers: {
        projects: project,
        'api-token': Cypress.env('ADMIN_TOKEN')
      },
      method: 'GET',
      url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
    })
    .then((resp: Cypress.ObjectLike) => {
      // to avoid getting stuck in an infinite loop
      if (maxRetries === 0) {
        return;
      }
      if (resp.body.length > 0 ) {
        return;
      }
      cy.wait(1000);
      waitForNodes(project, maxRetries - 1);
    });
}

function uuidv4() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    // tslint:disable 
    const r = Math.random() * 16 | 0;
    // tslint:disable 
    const v = c === 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}
