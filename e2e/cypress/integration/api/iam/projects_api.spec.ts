import { Rule, Project } from '../../../support/types';

// these tests are best read sequentially, as they share state
describe('projects API', () => {
  const cypressPrefix = 'test-projects-api';

  const projectWithOrgRule: Project = {
    id: `${cypressPrefix}-project1-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'Test Avengers Project',
    skip_policies: true
  };

  const projectWithServerRule: Project = {
    id: `${cypressPrefix}-project2-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'Test X-men Project',
    skip_policies: true
  };

  const orgRule: Rule = {
    id: 'org-rule-1',
    name: 'first rule of avengers project',
    type: 'NODE',
    project_id: projectWithOrgRule.id,
    conditions: [
      {
        attribute: 'CHEF_ORGANIZATION',
        operator: 'EQUALS',
        values: ['avengers']
      }
    ]
  };

  const serverRule: Rule = {
    id: 'server-rule-1',
    name: 'first rule of xmen project',
    type: 'NODE',
    project_id: projectWithServerRule.id,
    conditions: [
      {
        attribute: 'CHEF_SERVER',
        operator: 'EQUALS',
        values: ['xmen.co']
      }
    ]
  };

  before(() => {
    // Cypress recommends state cleanup in the before block to ensure
    // it gets run every time:
    // tslint:disable-next-line:max-line-length
    // https://docs.cypress.io/guides/references/best-practices.html#Using-after-or-afterEach-hooks
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects']);
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects']);
  });

  describe('applying project rules', () => {

    // testing values
    const noRulesStr = 'NO_RULES';
    const editsPendingStr = 'EDITS_PENDING';
    const rulesAppliedStr = 'RULES_APPLIED';
    // corresponds to the number of nodes ingested in this test via fixtures.
    const numberOfNodesAdded = 4;

    before(() => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/ingest/events/chef/node-multiple-deletes',
        body: {
          node_ids: [
            'f6a5c33f-bef5-433b-815e-a8f6e69e6b1b',
            '82760210-4686-497e-b039-efca78dee64b',
            '9c139ad0-89a5-44bc-942c-d7f248b155ba',
            '6453a764-2415-4934-8cee-2a008834a74a'
          ]
        },
        failOnStatusCode: false
      });

      for (const project of [projectWithOrgRule, projectWithServerRule]) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: '/apis/iam/v2/projects',
          body: project
        });
      }

      let totalNodes = 0;
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: '(unassigned)'
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        totalNodes = response.body.length;
      });

      cy.fixture('converge/avengers1.json').then(node1 => {
        cy.fixture('converge/avengers2.json').then(node2 => {
          cy.fixture('converge/xmen1.json').then(node3 => {
            cy.fixture('converge/xmen2.json').then(node4 => {
              for (const node of [node1, node2, node3, node4]) {
                cy.sendToDataCollector(node);
              }
            });
          });
        });
      });

      const maxRetries = 200;
      waitForUnassignedNodes(totalNodes + numberOfNodesAdded, maxRetries);

      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithOrgRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0);
      });

      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithServerRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0);
      });
    });

    it('new rules get applied to nodes', () => {
      // initially no rules
      for (const project of [projectWithOrgRule, projectWithServerRule]) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}/rules`
        }).then((response) => {
          expect(response.body.rules).to.have.length(0);
        });

        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}`
        }).then((response) => {
          expect(response.body.project.status).to.equal(noRulesStr);
        });
      }

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => {
          return id === projectWithOrgRule.id || id === projectWithServerRule.id;
        })
          .forEach(({ status }) => expect(status).to.equal(noRulesStr));
      });

      for (const rule of [orgRule, serverRule]) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: `/apis/iam/v2/projects/${rule.project_id}/rules`,
          body: rule
        });
      }

      // confirm rules are staged
      for (const project of [projectWithOrgRule, projectWithServerRule]) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}/rules`
        }).then((response) => {
          expect(response.body.rules).to.have.length(1);
          for (const rule of response.body.rules) {
            expect(rule).to.have.property('status', 'STAGED');
          }
        });

        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}`
        }).then((response) => {
          expect(response.body.project.status).to.equal(editsPendingStr);
        });
      }

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => {
          return id === projectWithOrgRule.id || id === projectWithServerRule.id;
        })
          .forEach(({ status }) => expect(status).to.equal(editsPendingStr));
      });

      cy.applyRulesAndWait();

      // confirm rules are applied
      for (const project of [projectWithOrgRule, projectWithServerRule]) {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}/rules`
        }).then((response) => {
          expect(response.body.rules).to.have.length(1);
          for (const rule of response.body.rules) {
            expect(rule).to.have.property('status', 'APPLIED');
          }
        });

        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'GET',
          url: `/apis/iam/v2/projects/${project.id}`
        }).then((response) => {
          expect(response.body.project.status).to.equal(rulesAppliedStr);
        });
      }

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => {
          return id === projectWithOrgRule.id || id === projectWithServerRule.id;
        })
          .forEach(({ status }) => expect(status).to.equal(rulesAppliedStr));
      });

      // confirm nodes are assigned to projects correctly
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithOrgRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2);
      });

      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithServerRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2);
      });
    });

    it('rules with updated conditions get applied to nodes', () => {

      // change avengers rule to include both organizations
      const updatedorgRule = orgRule;

      updatedorgRule.conditions = [
        {
          attribute: 'CHEF_ORGANIZATION',
          operator: 'MEMBER_OF',
          values: ['avengers', 'xmen']
        }
      ];

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(rulesAppliedStr);
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => id === projectWithOrgRule.id)
          .forEach(({ status }) => expect(status).to.equal(rulesAppliedStr));
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'PUT',
        url: `/apis/iam/v2/projects/${orgRule.project_id}/rules/${orgRule.id}`,
        body: updatedorgRule
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(editsPendingStr);
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => id === projectWithOrgRule.id)
          .forEach(({ status }) => expect(status).to.equal(editsPendingStr));
      });

      cy.applyRulesAndWait();

      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithOrgRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(4);
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(rulesAppliedStr);
      });

      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        const projects: Project[] = response.body.projects;
        projects.filter(({ id, status }) => id === projectWithOrgRule.id)
          .forEach(({ status }) => expect(status).to.equal(rulesAppliedStr));
      });
    });

    it('deleted rules get applied to nodes', () => {
      // verify project has applied rules
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(rulesAppliedStr);
      });

      // delete the project's rule
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${orgRule.project_id}/rules/${orgRule.id}`
      });

      // verify project now has edits pending
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(editsPendingStr);
      });

      cy.applyRulesAndWait();

      // verify the project no longer has any rules
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((response) => {
        expect(response.body.project.status).to.equal(noRulesStr);
      });

      // verify the project is no longer applied to node data
      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: projectWithOrgRule.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0);
      });

      cy.request({
        headers: {
          'api-token': Cypress.env('ADMIN_TOKEN'),
          projects: '(unassigned)'
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2);
      });
    });
  });

  describe('project graveyarding and deletion', () => {
    it('gives a 404 when the project does not exist', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        url: '/apis/iam/v2/projects/not_found',
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.equal(404);
      });
    });

    it('fails to delete project with staged or applied rules', () => {

      // Try to delete a project with an applied rule
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${projectWithServerRule.id}`,
        failOnStatusCode: false
      }).then((deleteResp) => {
        expect(deleteResp.status,
          'Fails to delete a project with an applied rule').to.equal(400);
      });

      // Add a new staged rule to a project with existing rules
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: `/apis/iam/v2/projects/${projectWithServerRule.id}/rules`,
        body: {
          id: 'xmen-rule-2',
          name: 'second rule of xmen project',
          type: 'NODE',
          project_id: projectWithServerRule.id,
          conditions: [
            {
              attribute: 'CHEF_ORGANIZATION',
              operator: 'EQUALS',
              values: ['xmen']
            }
          ]
        }
      });

      // Try to delete the project with a staged rule
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${projectWithServerRule.id}`,
        failOnStatusCode: false
      }).then((deleteResp) => {
        expect(deleteResp.status,
          'Fails to delete a project with a staged rule').to.equal(400);
      });

      // Delete an applied rule
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${projectWithServerRule.id}/rules/${serverRule.id}`
      });

      // Try to delete a project with a deleted rule that has not been applied
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${projectWithServerRule.id}`,
        failOnStatusCode: false
      }).then((deleteResp) => {
        expect(deleteResp.status,
          'Fails to delete a project with a deleted rule that has not been applied')
          .to.equal(400);
      });
    });

    it('successfully deletes a project with no rules', () => {

      // Try deleting a project with no rules
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'DELETE',
        url: `/apis/iam/v2/projects/${projectWithOrgRule.id}`
      }).then((deleteResp) => {
        expect(deleteResp.status).to.equal(200);
      });

      // Ensure the project is removed
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'GET',
        url: '/apis/iam/v2/projects'
      }).then((resp: Cypress.ObjectLike) => {
        expect(
          responseContainsProject(resp.body.projects, projectWithOrgRule.id)).to.equal(false);
      });

      waitUntilProjectNotInGraveyard(projectWithOrgRule.id, 100);
    });
  });
});

function waitForUnassignedNodes(totalNodes: number, maxRetries: number) {
  cy
    .request({
      headers: {
        projects: '(unassigned)',
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
      if (resp.body.length === totalNodes + 4) {
        return;
      }

      waitForUnassignedNodes(totalNodes, maxRetries - 1);
    });
}

function waitUntilProjectNotInGraveyard(projectID: string, attempts: number): void {
  if (attempts === -1) {
    throw new Error('project-delete never finished');
  }
  cy.request({
    headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
    url: '/apis/iam/v2/projects',
    method: 'POST',
    failOnStatusCode: false,
    body: {
      id: projectID,
      name: 'can we create yet',
      skip_policies: true
    }
  }).then((response) => {
    if (response.status === 409) {
      return;
    } else {
      cy.log(`${attempts} attempts remaining: waiting for project with ` +
      `id ${projectID} to be removed from graveyard`);
      cy.wait(1000);
      waitUntilProjectNotInGraveyard(projectID, --attempts);
    }
  });
}

function responseContainsProject(projectsResponse: any, projectId: string): boolean {
  return projectsResponse &&
    projectsResponse.length > 0 &&
    projectsResponse.some((p: any) => p.id === projectId);
}
