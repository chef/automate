import { describeIfIAMV2p1 } from '../../constants';

describeIfIAMV2p1('Client Runs Ingestion project tagging', () => {
  const cypressPrefix = 'test-projects-api-client-runs';

  const projectsWithRule = [
    // This test is commented out because there is a current limit of 6 projects allowed
    // {
    //   project: {
    //     id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
    //     name: 'project org'
    //   },
    //   rule: {
    //     id: 'rule-org',
    //     name: 'rule CHEF_ORGANIZATION',
    //     type: 'NODE',
    //     project_id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
    //     conditions: [
    //       {
    //         attribute: 'CHEF_ORGANIZATION',
    //         operator: 'EQUALS',
    //         values: ['75th Rangers']
    //       }
    //     ]
    //   }
    // },
    {
      project: {
        id: `${cypressPrefix}-project-chef-server-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project chef server'
      },
      rule: {
        id: 'rule-chef-server',
        name: 'rule CHEF_SERVER',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-chef-server-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_SERVER',
            operator: 'EQUALS',
            values: ['example.org']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-project-environment-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project environment'
      },
      rule: {
        id: 'rule-environment',
        name: 'rule ENVIRONMENT',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-environment-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'ENVIRONMENT',
            operator: 'EQUALS',
            values: ['arctic']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-project-policy-group-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project policy group'
      },
      rule: {
        id: 'rule-policy-group',
        name: 'rule CHEF_POLICY_GROUP',
        type: 'NODE',
        project_id:
        `${cypressPrefix}-project-policy-group-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_POLICY_GROUP',
            operator: 'EQUALS',
            values: ['red_ring']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-project-policy-name-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project policy name'
      },
      rule: {
        id: 'rule-policy-name',
        name: 'rule CHEF_POLICY_NAME',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-policy-name-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_POLICY_NAME',
            operator: 'EQUALS',
            values: ['fire']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-project-role-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project role'
      },
      rule: {
        id: 'rule-role',
        name: 'rule CHEF_ROLE',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-role-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_ROLE',
            operator: 'EQUALS',
            values: ['backend']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-project-tag-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project tag'
      },
      rule: {
        id: 'rule-tag',
        name: 'rule CHEF_TAG',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-tag-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_TAG',
            operator: 'EQUALS',
            values: ['v3']
          }
        ]
      }
    }
  ];

  before(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    // create the projects with one rule
    projectsWithRule.forEach(projectWithRule => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2beta/projects',
        body: projectWithRule.project
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2beta/projects/${projectWithRule.rule.project_id}/rules`,
        body: projectWithRule.rule
      });
    });

    // apply rules
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2beta/apply-rules'
    });

    waitUntilApplyRulesNotRunning(100);
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  it('tagging 7 projects with all the attributes on a client runs node', () => {
    const nodeId = uuidv4();

    // Ensure there are no nodes matching any of the projects
    projectsWithRule.forEach(projectWithRule => {
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithRule.project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0);
      });
    });

    // Ingest a node with attribues that match all the projects
    cy.fixture('converge/avengers1.json').then((node) => {
      node.organization_name = '75th Rangers';
      node.chef_server_fqdn = 'example.org';
      node.node.chef_environment = 'arctic';
      node.policy_group = 'red_ring';
      node.policy_name = 'fire';
      node.node.automatic.roles = ['backend'];
      node.node.normal.tags = ['v3'];
      node.entity_uuid = nodeId;
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });

    // wait for the node to be ingested
    waitForNodes(projectsWithRule[0].project.id, 30);

    // Ensure the node is tagged with the correct project
    projectsWithRule.forEach(projectWithRule => {
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithRule.project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(1);
        expect(response.body[0].id).to.equal(nodeId);
      });
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
  });
});

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
