import { uuidv4 } from '../../../support/helpers';

describe('Action project tagging on ingestion', () => {
  const cypressPrefix = 'test-client-runs-update';
  const actionId = uuidv4();
  const entityName = `ingest-action-${Cypress.moment().format('MMDDYYhhmmss')}`;

  const projectsWithRule = [
    {
      project: {
        id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project org'
      },
      rule: {
        id: 'rule-org',
        name: 'Chef Organization',
        type: 'EVENT',
        project_id: `${cypressPrefix}-project-org-${Cypress.moment().format('MMDDYYhhmm')}`,
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
        id: `${cypressPrefix}-project-chef-server-${Cypress.moment().format('MMDDYYhhmm')}`,
        name: 'project chef server'
      },
      rule: {
        id: 'rule-chef-server',
        name: 'Chef Server',
        type: 'EVENT',
        project_id: `${cypressPrefix}-project-chef-server-${Cypress.moment().format('MMDDYYhhmm')}`,
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

  // we run most of the API calls, including the time-intensive apply-rules,
  // in the before block so we only have to run them once
  before(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);

    // create the projects with one rule
    projectsWithRule.forEach(projectWithRule => {
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

    cy.applyRulesAndWait(100);

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

    // wait for the action to be ingested
    waitForAction(entityName, 30);
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  projectsWithRule.forEach((projectWithRule) => {
    const attribute = projectWithRule.rule.name;
    it(`when a project has a rule that matches an action's ${attribute},
    successfully associates that action with the project`, () => {
      const end = Cypress.moment().utc().endOf('day').valueOf().toString();
      const start = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();

      // Ensure the action is tagged with the correct project
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithRule.project.id
        },
        method: 'GET',
        url: `api/v0/eventfeed?collapse=false&page_size=100&start=${start}&end=${end}`
      }).then((response) => {
        expect(response.body.events.length).to.greaterThan(0);
        expect(eventExist(entityName, response.body.events)).to.equal(true);
      });
    });
  });
});

function waitForAction(entityName: string, maxRetries: number) {
  const end = Cypress.moment().utc().endOf('day').valueOf().toString();
  const start = Cypress.moment().utc().subtract(3, 'day').valueOf().toString();

  cy.request({
    headers: {
      projects: ['*'],
      'api-token': Cypress.env('ADMIN_TOKEN')
    },
    method: 'GET',
    url: `api/v0/eventfeed?collapse=false&page_size=100&start=${start}&end=${end}`
  })
    .then((resp: Cypress.ObjectLike) => {
      // to avoid getting stuck in an infinite loop
      if (maxRetries === 0) {
        expect(0).to.equal(1);
        return;
      }
      if (resp.body.events && resp.body.events.length > 0 &&
        eventExist(entityName, resp.body.events)) {
        return;
      }
      cy.wait(1000);
      waitForAction(entityName, maxRetries - 1);
    });
}

function eventExist(entityName: string, events: any[]): boolean {
  for (const event of events) {
    if (event.entity_name === entityName) {
      return true;
    }
  }

  return false;
}
