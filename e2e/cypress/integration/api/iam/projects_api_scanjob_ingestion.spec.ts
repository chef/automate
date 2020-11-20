import { Rule, Project } from '../../../support/types';

describe('ScanJob Ingestion project tagging', () => {
  const cypressPrefix = 'test-sj-ing-proj';
  const nodeStart = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
  const nodeEnd = Cypress.moment().utc().endOf('day').format();
  const now = Cypress.moment().format('MMDDYYhhmm');
  const projectId = `${cypressPrefix}-projects-${now}`;
  const environment = 'test-env';
  const nodeName = `my-ssh-node-${now}`;
  const secretName = `test-cred-${now}`;
  const scanJobName = `test-scanjob-${now}`;

  const targetKey = atob(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_KEY'))
    .replace('-----BEGIN RSA PRIVATE KEY----- ', '').replace('-----END RSA PRIVATE KEY----- ', '');
  const targetHost = Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_HOST');
  const targetUser = Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_USER');

  interface ProjectAndRule {
    project: Project;
    rule: Rule;
  }

  const projectWithNodeRules: ProjectAndRule = {
    project: {
      id: projectId,
      name: 'project environment',
      skip_policies: true
    },
    rule: {
      id: 'rule-environment-3',
      name: 'rule ENVIRONMENT',
      type: 'NODE',
      project_id: projectId,
      conditions: [
        {
          attribute: 'ENVIRONMENT',
          operator: 'EQUALS',
          values: [environment]
        }
      ]
    }
  };

  before(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    // create the projects with one node rule each
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/projects',
      body: projectWithNodeRules.project
    });

    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: `/apis/iam/v2/projects/${projectWithNodeRules.rule.project_id}/rules`,
      body: projectWithNodeRules.rule
    });

    cy.applyRulesAndWait();
  });

  after(() => {
    // Delete the created secret
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/api/v0/secrets/search',
      body: '{}'
    }).then((response: Cypress.ObjectLike) => {
      response.body.secrets.forEach((secret: any) => {
        if (secret.name === secretName) {
          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'DELETE',
            url: `/api/v0/secrets/id/${response.body.secrets[0].id}`,
            body: {}
          });
        }
      });
    });

    // Delete the created node
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
    }).then((resp: Cypress.ObjectLike) => {
      resp.body.nodes.forEach((node: any) => {
        if (node.name === nodeName ) {
          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'DELETE',
            url: `/api/v0/nodes/id/${node.id}`,
            body: {}
          });
        }
      });
    });

    // delete scan job
    cy.request({
      headers: {
        projects: ['*'],
        'api-token': Cypress.env('ADMIN_TOKEN')
      },
      method: 'POST',
      url: '/api/v0/compliance/scanner/jobs/search',
      body: {
        filters: [
          { key: 'job_type', values: ['exec']},
          { key: 'parent_job', values: ['']}
        ],
        page: 1,
        per_page: 100,
        sort: 'end_time',
        order: 'DESC'
      }
    }).then((resp: Cypress.ObjectLike) => {
      resp.body.jobs.forEach((job: any) => {
        if (job.name === scanJobName ) {
          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'DELETE',
            url: `/api/v0/compliance/scanner/jobs/id/${job.id}`,
            body: {}
          });
        }
      });
    });
  });

  it.skip('when a project has a rule that matches a node\'s environment' +
  ' successfully associates that node with the project', () => {

    // Create a secret
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/api/v0/secrets',
      body: {
        'name': secretName,
        'type': 'ssh',
        'data': [
          {'key': 'username', 'value': targetUser},
          {'key': 'key', 'value': targetKey}
        ],
        'tags': []
      }
    });

    // get the secret - do I need a secret?
    cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/api/v0/secrets/search',
        body: {}
    }).then((getSecretResponse: Cypress.ObjectLike) => {
      const secretId = getSecretResponse.body.secrets[0].id;
      // Create a node
      cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/api/v0/nodes',
          body: {
            'name': nodeName,
            'manager': 'automate',
            'target_config': {
              'backend': 'ssh',
              'host': targetHost,
              'secrets': [
                secretId
              ],
              'port': 22
            },
            'tags': [{ 'key': 'environment', 'value': environment }]
          }
      }).then((nodeCreateResponse: Cypress.ObjectLike) => {
        const nodeID = nodeCreateResponse.body.id;

        // Create a scan job for that node
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/api/v0/compliance/scanner/jobs',
          body: {
            'name': scanJobName,
            'tags': [],
            'type': 'exec',
            'nodes': [nodeID],
            'profiles': [
              'https://github.com/dev-sec/apache-baseline/archive/master.tar.gz'
            ],
            'retries': 1,
            'node_selectors': []
          }
        }).then(() => {
          // wait for the scan job to run and the report to be ingested
          cy.waitForComplianceNode(nodeID, nodeStart, nodeEnd);

          // Ensure the compliance node is tagged with the correct project
          // by fetching the node with the expected project filter
          cy.request({
            headers: {
              'api-token': Cypress.env('ADMIN_TOKEN'),
              projects: [projectWithNodeRules.project.id]
            },
            method: 'POST',
            url: '/api/v0/compliance/reporting/nodes/search',
            body: {
                filters: [
                  { type: 'start_time', values: [nodeStart]},
                  { type: 'end_time', values: [nodeEnd]},
                  { type: 'node_id', values: [nodeID]}
                ],
                order: 'DESC',
                page: 1,
                per_page: 100,
                sort: 'latest_report.end_time'
            }
          }).then((searchCompNodesResponse: Cypress.ObjectLike) => {
            expect(searchCompNodesResponse.body.nodes).to.have.length(1);
          });
        });
      });
    });
  });
});
