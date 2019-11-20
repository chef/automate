import { describeIfIAMV2p1 } from '../../constants';
import { uuidv4 } from '../../helpers';

describeIfIAMV2p1('Nodemanager project update tagging', () => {
  const cypressPrefix = 'test-nodemanager-update';


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
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  it('tagging 7 projects with all the attributes on a compliance and client runs node', () => {
    const complianceNodeId = uuidv4();
    const clientRunsNodeId = uuidv4();
    const reportId = uuidv4();
    const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmm')}`;
    const start = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
    const end = Cypress.moment().utc().endOf('day').format();

    // Ingest a InSpec report with attribues that match all the projects
    cy.fixture('compliance/inspec-report.json').then((report) => {
      report.organization_name = '75th Rangers';
      report.source_fqdn = 'example.org';
      report.environment = 'arctic';
      report.policy_group = 'red_ring';
      report.policy_name = 'fire';
      report.roles = ['backend'];
      report.chef_tags = ['v3'];
      report.node_uuid = complianceNodeId;
      report.node_name = nodeName;
      report.end_time = Cypress.moment().utc().format();
      report.report_uuid = reportId;
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: report
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
      node.entity_uuid = clientRunsNodeId;
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: node
      });
    });

    // wait for the report to be ingested
    waitForNodemanagerNode(complianceNodeId, 30);
    // wait for the client run report to be ingested
    waitForNodemanagerNode(clientRunsNodeId, 30);

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

    cy.waitUntilApplyRulesNotRunning(100);

    // Ensure that both the compliance and client run nodes' tags are updated
    projectsWithRule.forEach(projectWithRule => {
      cy.request({
        headers: {
          projects: [projectWithRule.project.id],
          'api-token': Cypress.env('ADMIN_TOKEN')
        },
        method: 'POST',
        url: '/api/v0/nodes/search',
        body: {
          order: 'DESC',
          page: 1,
          per_page: 100
        }
      }).then((response) => {
        expect(response.body.nodes.length).to.greaterThan(0);
        expect(nodeExists(complianceNodeId, response.body.nodes)).to.equal(true);
        expect(nodeExists(clientRunsNodeId, response.body.nodes)).to.equal(true);
      });
    });
  });
});

function nodeExists(nodeId: string, nodes: any[]): boolean {
  for (const node of nodes ) {
    if (node.id === nodeId) {
      return true;
    }
  }

  return false;
}

function waitForNodemanagerNode(nodeId: string, maxRetries: number) {
  cy.request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'POST',
    url: '/api/v0/nodes/search',
    body: {
      order: 'DESC',
      page: 1,
      per_page: 100
    }
  })
  .then((resp: Cypress.ObjectLike) => {
    // to avoid getting stuck in an infinite loop
    if (maxRetries === 0) {
      return;
    }
    if (resp.body.nodes && resp.body.nodes.length > 0 && nodeExists(nodeId, resp.body.nodes)) {
      return;
    }
    cy.wait(1000);
    waitForNodemanagerNode(nodeId, maxRetries - 1);
  });
}
