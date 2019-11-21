import { describeIfIAMV2p1 } from '../../constants';
import { uuidv4 } from '../../helpers';

describeIfIAMV2p1('project update re-tagging', () => {
  const cypressPrefix = 'test-node-update';

  const projectsWithRule = [
    {
      project: {
        id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project org'
      },
      rule: {
        id: 'rule-org',
        name: 'rule CHEF_ORGANIZATION',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
        conditions: [
          {
            attribute: 'CHEF_ORGANIZATION',
            operator: 'EQUALS',
            values: ['75th Rangers']
          }
        ]
      }
    },
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

  const complianceNodeId = uuidv4();
  const clientRunsNodeId = uuidv4();
  const reportId = uuidv4();
  const nodeName = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmm')}`;
  const start = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
  const end = Cypress.moment().utc().endOf('day').format();

  before(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

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
    cy.waitForNodemanagerNode(complianceNodeId, 30);
    // wait for the client run report to be ingested
    cy.waitForNodemanagerNode(clientRunsNodeId, 30);

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
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  describe('Node Manager', () => {
    for ( const projectWithRule of projectsWithRule ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

        // Ensure that both the compliance and client run nodes' tags are updated
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
          expect(response.body.nodes.length).to.be.greaterThan(0);
          expect(response.body.nodes.some((node: any) =>
            node.id === complianceNodeId)).to.equal(true);
          expect(response.body.nodes.some((node: any) =>
            node.id === clientRunsNodeId)).to.equal(true);
        });
      });
    }
  });

  describe('Client Runs', () => {
    for ( const projectWithRule of projectsWithRule ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

        // Ensure that both the compliance and client run nodes' tags are updated
        cy.request({
          headers: {
            'api-token': Cypress.env('ADMIN_TOKEN'),
            projects: projectWithRule.project.id
          },
          method: 'GET',
          url: `/api/v0/cfgmgmt/nodes?pagination.size=10&filter=node_id:${clientRunsNodeId}`
        }).then((response) => {
          expect(response.body).to.have.length(1);
          expect(response.body[0].id).to.equal(clientRunsNodeId);
        });
      });
    }
  });

  describe('Compliance Runs', () => {
    for ( const projectWithRule of projectsWithRule ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

        // Ensure the node is tagged with the correct project
        cy.request({
          headers: {
            'api-token': Cypress.env('ADMIN_TOKEN'),
            projects: [projectWithRule.project.id]
          },
          method: 'POST',
          url: '/api/v0/compliance/reporting/nodes/search',
          body: {
            filters: [
              { type: 'start_time', values: [start]},
              { type: 'end_time', values: [end]},
              { type: 'node_id', values: [complianceNodeId]}
            ],
            order: 'DESC',
            page: 1,
            per_page: 100,
            sort: 'latest_report.end_time'
          }
        }).then((response) => {
          expect(response.body.nodes).to.have.length(1);
        });
      });
    }
  });
});
