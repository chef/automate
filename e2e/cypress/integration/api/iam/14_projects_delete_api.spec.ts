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

    // Try to delete the project with a stagged rule
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a stagged rule').to.be.oneOf([400]);
    });

    // Ensure the project was not deleted
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && resp.body.projects.length > 0 &&
        resp.body.projects.some((p: any) => p.id === project.id)).to.be.true;
    });

    // Apply the rules to make the rule applied
    cy.applyRulesAndWait(100);

    // Try to delete the project with the applied rule
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a applied rule').to.be.oneOf([400]);
    });

    // Ensure the project was not removed
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

    // Try to delete the project with a non applied deleted rule
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'DELETE',
      url: `/apis/iam/v2beta/projects/${project.id}`,
      failOnStatusCode: false
    }).then((deleteResp) => {
      expect(deleteResp.status,
        'Did not fail deleting a project with a deleted applied rule').to.be.oneOf([400]);
    });

    // Ensure the project was not removed
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'GET',
      url: '/apis/iam/v2beta/projects'
    }).then((resp: Cypress.ObjectLike) => {
      expect(resp.body.projects && resp.body.projects.length > 0 &&
        resp.body.projects.some((p: any) => p.id === project.id)).to.be.true;
    });

    // Apply the rules to remove the rule
    cy.applyRulesAndWait(100);

    // Now deleting the project should work
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
