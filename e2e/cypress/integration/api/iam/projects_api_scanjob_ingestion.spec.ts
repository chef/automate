import { Rule, Project } from '../../../support/types';

const nodeStart = Cypress.moment().utc().subtract(3, 'day').startOf('day').format();
const nodeEnd = Cypress.moment().utc().endOf('day').format();
const now = Cypress.moment().format('MMDDYYhhmm');
const decoded = atob(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_KEY'));
const host = Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_HOST');


describe('ScanJob Ingestion project tagging', () => {
  const cypressPrefix = 'test-scanjob-ingestion-projects';
  interface ProjectAndRule {
    project: Project;
    rule: Rule;
  }

  const projectsWithNodeRules: ProjectAndRule[] = [
    {
      project: {
        id: 'test-scanjob-projects-2',
        name: 'project environment',
        skip_policies: true
      },
      rule: {
        id: 'rule-environment-2',
        name: 'rule ENVIRONMENT',
        type: 'NODE',
        project_id: 'test-scanjob-projects-2',
        conditions: [
          {
            attribute: 'ENVIRONMENT',
            operator: 'EQUALS',
            values: ['test-env']
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

    cy.applyRulesAndWait(100);
  });

  describe('Compliance Runs', () => {
    for (const projectWithRule of projectsWithNodeRules ) {
      it(`when a project has a rule that matches a node's ${projectWithRule.rule.name},
      successfully associates that node with the project`, () => {

    // Create a credential, node, then trigger scan job on node 
    cy.route('POST', '/api/v0/secrets/search').as('getSecrets');

    cy.get('.nav-link').contains('Settings').click();
    cy.url().should('include', '/settings');

    // add force to avoid waiting on the whole page to load
    cy.get('chef-sidebar-entry').contains('Node Credentials').click({ force: true });

    // click on add credential button to open create form
    cy.contains('Add Credential').click();
    cy.url().should('include', '/settings/node-credentials/add');

    // fill in name for credential, username, and key
    cy.get('form input[formcontrolname="name"]').first().type("test-cred");
    cy.get('form input[formcontrolname="username"]').first()
      .type(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_USER'));

    // we save some time by setting this long string as the value rather than typing it out
    cy.get('form textarea[formcontrolname="key"]').clear().invoke('val', decoded).trigger('input');

    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/secrets').as('createSecret');

    // save the credential
    cy.contains('Save').click();

    cy.url().should('include', '/settings/node-credentials');

    // wait for data to return
    cy.wait('@createSecret');
    cy.wait('@getSecrets');

    cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/api/v0/secrets/search',
        body: '{}'
    }).then((response) => {
        const node = `{"name": "my-ssh-node", "manager":"automate", "target_config": {"backend":"ssh", "host": "${host}","secrets":["${response.body.secrets[0].id}"],"port": 22},"tags": [{ "key":"environment", "value":"test-env" }]}`;
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/api/v0/nodes',
            body: node
        }).then((response) => {
            const nodeID = response.body.id
            const job = `{"name": "test job", "tags": [], "type": "exec", "nodes": ["${nodeID}"], "profiles": ["https://github.com/dev-sec/linux-baseline/archive/master.tar.gz", "https://github.com/dev-sec/ssh-baseline/archive/master.tar.gz"], "retries": 1, "node_selectors": []}`;
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/api/v0/compliance/scanner/jobs',
                body: job
            }).then((response) => {
                // wait for the report to be ingested
                cy.waitForComplianceNode(nodeID, nodeStart, nodeEnd, 120);
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
                    { type: 'node_id', values: [nodeID]}
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
        });
    });
    });
    }
  });
});
