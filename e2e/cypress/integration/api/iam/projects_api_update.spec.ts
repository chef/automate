import { eventExist, uuidv4 } from '../../../support/helpers';
import { Rule, Project } from '../../../support/types';

describe('project update re-tagging', () => {
  const cypressPrefix = 'proj-update';
  const now = Cypress.moment().format('MMDDYYhhmm');

  const nodeOrgProjectID = `${cypressPrefix}-proj-org-${now}`;
  const nodeServerProjectID = `${cypressPrefix}-proj-server-${now}`;
  const nodeEnvProjectID = `${cypressPrefix}-proj-env-${now}`;
  const nodePolGroupProjectID = `${cypressPrefix}-proj-pol-group-${now}`;
  const nodePolNameProjectID = `${cypressPrefix}-proj-pol-name-${now}`;
  const nodeRoleProjectID = `${cypressPrefix}-proj-role-${now}`;
  const nodeTagProjectID = `${cypressPrefix}-proj-tag-${now}`;

  interface ProjectAndRule {
    project: Project;
    rule: Rule;
  }

  const projectsWithNodeRule: ProjectAndRule[] = [
    {
      project: {
        id: nodeOrgProjectID,
        name: 'project org',
        skip_policies: true
      },
      rule: {
        id: 'rule-org',
        name: 'rule CHEF_ORGANIZATION',
        type: 'NODE',
        project_id: nodeOrgProjectID,
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
        id: nodeServerProjectID,
        name: 'project chef server',
        skip_policies: true
      },
      rule: {
        id: 'rule-chef-server',
        name: 'rule CHEF_SERVER',
        type: 'NODE',
        project_id: nodeServerProjectID,
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
        id: nodeEnvProjectID,
        name: 'project environment',
        skip_policies: true
      },
      rule: {
        id: 'rule-environment',
        name: 'rule ENVIRONMENT',
        type: 'NODE',
        project_id: nodeEnvProjectID,
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
        id: nodePolGroupProjectID,
        name: 'project policy group',
        skip_policies: true
      },
      rule: {
        id: 'rule-policy-group',
        name: 'rule CHEF_POLICY_GROUP',
        type: 'NODE',
        project_id: nodePolGroupProjectID,
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
        id: nodePolNameProjectID,
        name: 'project policy name',
        skip_policies: true
      },
      rule: {
        id: 'rule-policy-name',
        name: 'rule CHEF_POLICY_NAME',
        type: 'NODE',
        project_id: nodePolNameProjectID,
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
        id: nodeRoleProjectID,
        name: 'project role',
        skip_policies: true
      },
      rule: {
        id: 'rule-role',
        name: 'rule CHEF_ROLE',
        type: 'NODE',
        project_id: nodeRoleProjectID,
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
        id: nodeTagProjectID,
        name: 'project tag',
        skip_policies: true
      },
      rule: {
        id: 'rule-tag',
        name: 'rule CHEF_TAG',
        type: 'NODE',
        project_id: nodeTagProjectID,
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

  const eventOrgProjectID = `${cypressPrefix}-proj-evt-org-${now}`;
  const eventServerProjectID = `${cypressPrefix}-proj-evt-server-${now}`;

  const projectsWithEventRule = [
    {
      project: {
        id: eventOrgProjectID,
        name: 'project org',
        skip_policies: true
      },
      rule: {
        id: 'event-rule-org',
        name: 'rule CHEF_ORGANIZATION',
        type: 'EVENT',
        project_id: eventOrgProjectID,
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
        id: eventServerProjectID,
        name: 'project chef server',
        skip_policies: true
      },
      rule: {
        id: 'event-rule-chef-server',
        name: 'rule CHEF_SERVER',
        type: 'EVENT',
        project_id: eventServerProjectID,
        conditions: [
          {
            attribute: 'CHEF_SERVER',
            operator: 'EQUALS',
            values: ['example.org']
          }
        ]
      }
    }
  ];

  const complianceNodeId = uuidv4();
  const clientRunsNodeId = uuidv4();
  const reportId = uuidv4();
  const nodeName = `${cypressPrefix}-${now}`;
  const nodeStart = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
  const nodeEnd = Cypress.moment().utc().endOf('day').format();

  const actionStart = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();
  const actionEnd = Cypress.moment().utc().endOf('day').valueOf().toString();
  const actionId = uuidv4();
  const entityName = `entity_name-${Cypress.moment().format('MMDDYYhhmmss')}`;

  before(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

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

      cy.sendToDataCollector(report);
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

      cy.sendToDataCollector(node);
    });

    // Ingest an action with attributes that match all the projects
    cy.fixture('action/environment_create.json').then((action) => {
      action.organization_name = '75th Rangers';
      action.service_hostname = 'example.org';
      action.id = actionId;
      action.entity_name = entityName;
      action.recorded_at = Cypress.moment().utc().subtract(1, 'day').format();

      cy.sendToDataCollector(action);
    });

    // wait for the report to be ingested
    cy.waitForNodemanagerNode(complianceNodeId);
    cy.waitForComplianceNode(complianceNodeId, nodeStart, nodeEnd);

    // wait for the client run report to be ingested
    cy.waitForNodemanagerNode(clientRunsNodeId);
    cy.waitForClientRunsNode(clientRunsNodeId);

    // wait for the action to be ingested
    cy.waitForAction(entityName, actionStart, actionEnd);

    // create the projects with one node rule
    projectsWithNodeRule.forEach(projectWithRule => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: projectWithRule.project
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2/projects/${projectWithRule.rule.project_id}/rules`,
        body: projectWithRule.rule
      });
    });

    // create the projects with one event rule
    projectsWithEventRule.forEach(projectWithRule => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: projectWithRule.project
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2/projects/${projectWithRule.rule.project_id}/rules`,
        body: projectWithRule.rule
      });
    });

    cy.applyRulesAndWait();
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
    for (const projectWithRule of projectsWithNodeRule) {
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
    for (const projectWithRule of projectsWithNodeRule) {
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
    for (const projectWithRule of projectsWithNodeRule) {
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
              { type: 'start_time', values: [nodeStart] },
              { type: 'end_time', values: [nodeEnd] },
              { type: 'node_id', values: [complianceNodeId] }
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
    for (const projectWithRule of projectsWithEventRule) {
      it(`when a project has a rule that matches an action's ${projectWithRule.rule.name},
      successfully associates that action with the project`, () => {

        // Ensure the event is tagged with the correct project
        cy.request({
          headers: {
            'api-token': Cypress.env('ADMIN_TOKEN'),
            projects: [projectWithRule.project.id]
          },
          method: 'GET',
          url: `api/v0/eventfeed?collapse=false&page_size=100&start=${actionStart}&end=${actionEnd}`
        }).then((response) => {
          expect(response.body.events.length).to.greaterThan(0);
          expect(eventExist(entityName, response.body.events)).to.equal(true);
        });
      });
    }
  });
});
