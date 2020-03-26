import { eventExist, uuidv4 } from '../../../support/helpers';
import { Rule, Project } from '../../../support/types';

const nodeStart = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
const nodeEnd = Cypress.moment().utc().endOf('day').format();
const eventStart = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();
const eventEnd = Cypress.moment().utc().endOf('day').valueOf().toString();
const now = Cypress.moment().format('MMDDYYhhmm');

describe('Ingestion project tagging', () => {
  const cypressPrefix = 'test-ingestion-projects';
  const complianceNodeId = uuidv4();
  const clientRunsNodeId = uuidv4();
  const reportId = uuidv4();
  const nodeName = `${cypressPrefix}-${now}`;
  const actionId = uuidv4();
  const entityName = `ingest-action-${Cypress.moment().format('MMDDYYhhmmss')}`;

  interface ProjectAndRule {
    project: Project;
    rule: Rule;
  }

  const projectsWithNodeRules: ProjectAndRule[] = [
    {
      project: {
        id: `${cypressPrefix}-project-org-${now}`,
        name: 'project org',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-org',
        name: 'rule CHEF_ORGANIZATION',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-org-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-chef-server-${now}`,
        name: 'project chef server',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-chef-server',
        name: 'rule CHEF_SERVER',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-chef-server-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-environment-${now}`,
        name: 'project environment',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-environment',
        name: 'rule ENVIRONMENT',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-environment-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-policy-group-${now}`,
        name: 'project policy group',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-policy-group',
        name: 'rule CHEF_POLICY_GROUP',
        type: 'NODE',
        project_id:
        `${cypressPrefix}-project-policy-group-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-policy-name-${now}`,
        name: 'project policy name',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-policy-name',
        name: 'rule CHEF_POLICY_NAME',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-policy-name-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-role-${now}`,
        name: 'project role',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-role',
        name: 'rule CHEF_ROLE',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-role-${now}`,
        status: 'STAGED',
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
        id: `${cypressPrefix}-project-tag-${now}`,
        name: 'project tag',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'rule-tag',
        name: 'rule CHEF_TAG',
        type: 'NODE',
        project_id: `${cypressPrefix}-project-tag-${now}`,
        status: 'STAGED',
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

  const projectsWithEventRules = [
    {
      project: {
        id: `${cypressPrefix}-event-project-org-${now}`,
        name: 'event project org',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'event-rule-org',
        name: 'Chef Organization',
        type: 'EVENT',
        project_id: `${cypressPrefix}-event-project-org-${now}`,
        status: 'STAGED',
        conditions: [
          {
            attribute: 'CHEF_ORGANIZATION',
            operator: 'EQUALS',
            values: ['Team Pizza']
          }
        ]
      }
    },
    {
      project: {
        id: `${cypressPrefix}-event-project-chef-server-${now}`,
        name: 'event project chef server',
        type: 'CUSTOM',
        status: 'NO_RULES'
      },
      rule: {
        id: 'event-rule-chef-server',
        name: 'Chef Server',
        type: 'EVENT',
        project_id: `${cypressPrefix}-event-project-chef-server-${now}`,
        status: 'STAGED',
        conditions: [
          {
            attribute: 'CHEF_SERVER',
            operator: 'EQUALS',
            values: ['example.pizza']
          }
        ]
      }
    }
  ];

  before(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    // create the projects with one node rule each
    projectsWithNodeRules.forEach(project => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: project.project
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2/projects/${project.rule.project_id}/rules`,
        body: project.rule
      });
    });

    // create the projects with one event rule each
    projectsWithEventRules.forEach(project => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: project.project
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2/projects/${project.rule.project_id}/rules`,
        body: project.rule
      });
    });

    cy.applyRulesAndWait(100);

    // Ingest a InSpec report with attributes that match all the projects
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

    // Ingest a node with attributes that match all the projects
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

    // Ingest an action with attributes that match all the projects
    cy.fixture('action/environment_create.json').then((action) => {
      action.organization_name = 'Team Pizza';
      action.remote_hostname = 'example.pizza';
      action.id = actionId;
      action.entity_name = entityName;
      action.recorded_at = Cypress.moment().utc().subtract(1, 'day').format();

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/data-collector/v0',
        body: action
      });
    });

    // wait for the report to be ingested
    cy.waitForNodemanagerNode(complianceNodeId, 30);
    cy.waitForComplianceNode(complianceNodeId, nodeStart, nodeEnd, 30);

    // wait for the client run report to be ingested
    cy.waitForNodemanagerNode(clientRunsNodeId, 30);
    cy.waitForClientRunsNode(clientRunsNodeId, 30);

    // wait for the action to be ingested
    cy.waitForAction(entityName, eventStart, eventEnd, 30);
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/ingest/events/chef/node-multiple-deletes',
      body: {
        node_ids: [
          clientRunsNodeId,
          complianceNodeId
        ]
      },
      failOnStatusCode: false
    });
  });

  describe('Node Manager', () => {

    for (const projectWithRule of projectsWithNodeRules ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {
        // Ensure that both the compliance and client run nodes' tags are updated
        // by fetching the nodes with the expected project filter
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
    for (const projectWithRule of projectsWithNodeRules ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

        // Ensure that the cclient run nodes' tags are updated
        // by fetching the node with the expected project filter
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
    for (const projectWithRule of projectsWithNodeRules ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

        // Ensure the compliance node is tagged with the correct project
        // by fetching the node with the expected project filter
        cy.request({
          headers: {
            'api-token': Cypress.env('ADMIN_TOKEN'),
            projects: [projectWithRule.project.id]
          },
          method: 'POST',
          url: '/api/v0/compliance/reporting/nodes/search',
          body: {
            filters: [
              { type: 'start_time', values: [nodeStart]},
              { type: 'end_time', values: [nodeEnd]},
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

  describe('Actions', () => {
    const actionStart = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();
    const actionEnd = Cypress.moment().utc().endOf('day').valueOf().toString();

    projectsWithEventRules.forEach((projectWithRule) => {
      const attribute = projectWithRule.rule.name;
      it(`when a project has a rule that matches an action's ${attribute},
        successfully associates that action with the project`, () => {

        // Ensure the action is tagged with the correct project
        cy.request({
          headers: {
            'api-token': Cypress.env('ADMIN_TOKEN'),
            projects: projectWithRule.project.id
          },
          method: 'GET',
          url: `api/v0/eventfeed?collapse=false&page_size=100&start=${actionStart}&end=${actionEnd}`
        }).then((response) => {
          expect(response.body.events.length).to.be.greaterThan(0);
          expect(eventExist(entityName, response.body.events)).to.equal(true);
        });
      });
    });
  });
});
