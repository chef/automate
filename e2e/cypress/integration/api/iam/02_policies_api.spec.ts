import { describeIfIAMV2p1 } from '../../../support/constants';

describeIfIAMV2p1('policies API', () => {
  const defaultAdminReq = {
    headers: {}, // must fill in before use
    url: '/apis/iam/v2/policies'
  };
  const cypressPrefix = 'test-policies-api';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const project1 = {
      id: `${cypressPrefix}-project1-${now}`,
      name: 'Test Project 1'
  };
  const project2 = {
      id: `${cypressPrefix}-project2-${now}`,
      name: 'Test Project 2'
  };

  before(() => {
    defaultAdminReq.headers = { 'api-token': Cypress.env('ADMIN_TOKEN') };
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies', 'projects']);

    for (const project of [project1, project2]) {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        body: project
      });
    }
  });

  after(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies', 'projects']);
  });

  describe('POST /apis/iam/v2/policies', () => {
    beforeEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    afterEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    it('returns 400 when there are no statements',  () => {
      cy.request({
        ...defaultAdminReq,
        method: 'POST',
        failOnStatusCode: false,
        body: {
          id: `${cypressPrefix}-policy-${now}`,
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          projects: [project1.id]
        }
      }).then((response) => {
       expect(response.status).to.equal(400);
      });
    });
  });

  describe('PUT /apis/iam/v2/policies', () => {
    beforeEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    afterEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    const policyID = `${cypressPrefix}-policy-${now}`;
    beforeEach(() => {
      cy.request({
        ...defaultAdminReq,
        method: 'POST',
        body: {
          id: policyID,
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          statements: [
            {
              effect: 'ALLOW',
              actions: ['test:svc:someaction', 'test:svc:otheraction'],
              projects: [project1.id]
            }
          ],
          projects: [project1.id]
        }
      });
    });

    it('returns 400 when there are no statements',  () => {
      cy.request({
        ...defaultAdminReq,
        method: 'PUT',
        url: `/apis/iam/v2/policies/${policyID}`,
        failOnStatusCode: false,
        body: {
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          projects: [project1.id]
        }
      }).then((response) => {
       expect(response.status).to.equal(400);
      });
    });
  });

  describe('project assignment enforcement', () => {
    let nonAdminToken = '';
    const nonAdminTokenID = `${cypressPrefix}-nonadmin-token-${now}`;
    const statementProjects = [project1.id];
    const policyID = `${cypressPrefix}-policy-${now}`;
    const defaultNonAdminReq = {
      headers: {}, // must fill in before use
      method: 'POST',
      url: '/apis/iam/v2/policies'
    };

    before(() => {
      cy.request({
        ...defaultAdminReq,
        method: 'POST',
        url: '/apis/iam/v2/tokens',
        body: {
          id: nonAdminTokenID,
          name: 'Nonadmin Token'
        }
      }).then((response) => {
        nonAdminToken = response.body.token.value;
        defaultNonAdminReq.headers = { 'api-token': nonAdminToken };
      });

      cy.request({
        ...defaultAdminReq,
        method: 'POST',
        body: {
          name: 'token-access',
          id: `${cypressPrefix}-token-access-policy-${now}`,
          members: [`token:${nonAdminTokenID}`],
          statements: [
            {
              effect: 'ALLOW',
              actions: ['iam:policies:*'],
              projects: ['*']
            },
            {
              effect: 'ALLOW',
              actions: ['iam:projects:assign'],
              projects: [project1.id]
            }
          ],
          projects: []
        }
      });
    });

    after(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix,
        ['projects', 'policies', 'tokens']);
    });

    beforeEach(() => {
      cy.request({
        ...defaultAdminReq,
        method: 'DELETE',
        url: `/apis/iam/v2/policies/${policyID}`,
        failOnStatusCode: false
      });
    });

    afterEach(() => {
      cy.request({
        ...defaultAdminReq,
        method: 'DELETE',
        url: `/apis/iam/v2/policies/${policyID}`,
        failOnStatusCode: false
      });
    });

    describe('POST /apis/iam/v2/policies', () => {
      it('admin can create a new policy with no projects', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          body: policyWithProjects(policyID, [], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(0);
        });
      });

      it('admin can create a new policy with multiple projects', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(2);
        });
      });

      it('admin gets a 404 when it attempts to create ' +
        'a policy with a project that does not exist', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: policyWithProjects(policyID,
              [project1.id, project2.id, 'notfound'], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(404);
          });
        });

      it('non-admin with project1 assignment access but not (unassigned) access ' +
        'cannot create a new policy with no projects', () => {
          cy.request({
            ...defaultNonAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: policyWithProjects(policyID, [], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(403);
          });
        });

      it('non-admin with project1 assignment access can create ' +
        'a new policy with project1', () => {
          cy.request({
            ...defaultNonAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [project1.id], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(1);
          });
        });

      it('non-admin with project1 assignment access gets a 404 when it attempts to create ' +
        'a policy with a project that does not exist', () => {
          cy.request({
            ...defaultNonAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: policyWithProjects(policyID,
              [project1.id, project2.id, 'notfound'], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(404);
          });
        });

      it('non-admin with project1 assignment access cannot create ' +
        'a new policy with other projects (403)', () => {
          cy.request({
            ...defaultNonAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(403);
          });
        });
    });

    describe('PUT /apis/iam/v2/policies', () => {
      it('admin can update a policy with no projects to have projects', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          body: policyWithProjects(policyID, [], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(0);
        });

        cy.request({
          ...defaultAdminReq,
          method: 'PUT',
          url: `/apis/iam/v2/policies/${policyID}`,
          body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(2);
        });
      });

      it('admin can update a policy with projects to have no projects', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(2);
        });

        cy.request({
          ...defaultAdminReq,
          method: 'PUT',
          url: `/apis/iam/v2/policies/${policyID}`,
          body: policyWithProjects(policyID, [], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(0);
        });
      });

      it('admin cannot update a policy to have projects that do not exist', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          body: policyWithProjects(policyID, [], statementProjects)
        }).then((response) => {
          expect(response.body.policy.projects).to.have.length(0);
        });

        cy.request({
          ...defaultAdminReq,
          method: 'PUT',
          url: `/apis/iam/v2/policies/${policyID}`,
          failOnStatusCode: false,
          body: policyWithProjects(policyID, [project1.id, 'notfound'], statementProjects)
        }).then((response) => {
          expect(response.status).to.equal(404);
        });
      });

      it('non-admin with project1 assignment access can update ' +
        'a policy to remove project1', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(2);
          });

          cy.request({
            ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2/policies/${policyID}`,
            body: policyWithProjects(policyID, [project2.id], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(1);
          });
        });

      it('non-admin with project1 assignment access gets a 404 ' +
        'when updating a policy to have non-existent policies', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(0);
          });

          cy.request({
            ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2/policies/${policyID}`,
            failOnStatusCode: false,
            body: policyWithProjects(policyID, [project1.id, 'notfound'], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(404);
          });
        });

      it('non-admin with project1 assignment access cannot update ' +
        'a policy with no projects to have project1', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(0);
          });

          cy.request({
            ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2/policies/${policyID}`,
            failOnStatusCode: false,
            body: policyWithProjects(policyID, [project1.id], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(403);
          });
        });

      it('non-admin with project1 assignment access can update ' +
        'a policy with project1 to have no projects', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [project1.id], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(1);
          });

          cy.request({
            ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2/policies/${policyID}`,
            body: policyWithProjects(policyID, [], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(0);
          });
        });

      it('non-admin with project1 assignment access cannot update ' +
        'a policy to remove other projects', () => {
          cy.request({
            ...defaultAdminReq,
            method: 'POST',
            body: policyWithProjects(policyID, [project1.id, project2.id], statementProjects)
          }).then((response) => {
            expect(response.body.policy.projects).to.have.length(2);
          });

          cy.request({
            ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2/policies/${policyID}`,
            failOnStatusCode: false,
            body: policyWithProjects(policyID, [project1.id], statementProjects)
          }).then((response) => {
            expect(response.status).to.equal(403);
          });
        });
    });
  });
});

function policyWithProjects(id: string, projects: string[], statementProjects: string[]): any {

  return {
    id: id,
    name: 'fun name',
    members: ['team:local:test'],
    statements: [
      {
        effect: 'ALLOW',
        actions: ['test:svc:someaction', 'test:svc:otheraction'],
        projects: statementProjects
      }
    ],
    projects: projects
  };
}
