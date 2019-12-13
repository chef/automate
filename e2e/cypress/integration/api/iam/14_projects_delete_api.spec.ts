import { describeIfIAMV2p1 } from '../../../support/constants';
import { uuidv4 } from '../../../support/helpers';

describeIfIAMV2p1('Project delete', () => {
  const cypressPrefix = 'test-project-delete';

  const project =  {
    id: `${cypressPrefix}-project-delete-applied-rule${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'project delete'
  };

  const rule = {
    id: 'rule-applied',
    name: 'rule',
    type: 'NODE',
    project_id: project.id,
    conditions: [
      {
        attribute: 'CHEF_SERVER',
        operator: 'EQUALS',
        values: ['example.org']
      }
    ]
  };

  before(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']);
  });

  after(() => cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['projects', 'policies']));

  it('deleting a project', () => {
    // Create a project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2beta/projects',
      body: project
    });

    // Add a rule to the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: `/apis/iam/v2beta/projects/${project.id}/rules`,
      body: rule
    });

    // Try to delete the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a stagged rule').to.be.oneOf([400]);
    });

    // Ensure the project exists
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && resp.body.projects.length > 0 &&
        resp.body.projects.some((p: any) => p.id === project.id)).to.be.true;
    });

    applyRules();

    // Try to delete the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a applied rule').to.be.oneOf([400]);
    });

    // Ensure the project exists
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && resp.body.projects.length > 0 &&
        resp.body.projects.some((p: any) => p.id === project.id)).to.be.true;
    });

    // Delete the rule
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}/rules/${rule.id}`
    }).then((deleteResp) => {
      expect(deleteResp.status).to.be.oneOf([200]);
    });

    // Try to delete the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a deleted applied rule').to.be.oneOf([400]);
    });

    // Ensure the project exists
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && resp.body.projects.length > 0 &&
        resp.body.projects.some((p: any) => p.id === project.id)).to.be.true;
    });

    applyRules();

    // Delete the project
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`
    }).then((deleteResp) => {
      expect(deleteResp.status).to.be.oneOf([200]);
    });

    // Ensure the project is removed
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && (resp.body.projects.length === 0 ||
        !resp.body.projects.some((p: any) => p.id === project.id))).to.be.true;
    });
  });
});

function applyRules() {
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    method: 'POST',
    url: '/apis/iam/v2beta/apply-rules'
  });

  cy.waitUntilApplyRulesNotRunning(100);
}
