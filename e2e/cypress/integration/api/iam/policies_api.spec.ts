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
const policyID = `${cypressPrefix}-policy-${now}`;
const policyName = `${cypressPrefix} policy ${now}`;

const chefManagedPolicyIDs = [
  'administrator-access',
  'editor-access',
  'viewer-access',
  'ingest-access'
];

interface Policy {
  id: string;
  name: string;
  members: string[];
  statements: Statement[];
  projects: string[];
}

interface Statement {
  effect: string;
  actions: string[];
  projects: string[];
}


describe('policies API', () => {

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

  describe('policies API', () => {

    before(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    after(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['policies']);
    });

    describe('POST /apis/iam/v2/policies', () => {

      it('returns 200 when all valid inputs are provided', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'POST',
          failOnStatusCode: false,
          body: {
            id: policyID,
            name: policyName,
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
        }).then((response) => {
          expect(response.status).to.equal(200);
          expect(response.body.policy.id).to.equal(policyID);
          expect(response.body.policy.name).to.equal(policyName);
          expect(response.body.policy.members).to.deep.equal(['team:local:test']);
          expect(response.body.policy.statements).to.deep.equal([{
            effect: 'ALLOW',
            actions: ['test:svc:someaction', 'test:svc:otheraction'],
            resources: ['*'],
            role: '',
            projects: [project1.id]
          }]);
          expect(response.body.policy.projects).to.deep.equal([project1.id]);
        });
      });

      it('returns 400 when there are no statements', () => {
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

    describe('GET', () => {

      it('returns 200 on successful fetch (/apis/iam/v2/policies)', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'GET',
          url: '/apis/iam/v2/policies'
        }).then((response) => {
          expect(response.status).to.equal(200);
          expect(response.body.policies.length).to.equal(chefManagedPolicyIDs.length + 1);

          const policyIDs = response.body.policies.map((pol: Policy) => pol.id);

          chefManagedPolicyIDs.forEach((id: string) => {
            expect(policyIDs).include(id);
          });
          expect(policyIDs).to.include(policyID);
        });
      });

      it('returns 200 on successful fetch (/apis/iam/v2/policies/{policy_id})', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'GET',
          url: `/apis/iam/v2/policies/${policyID}`
        }).then((response) => {
          expect(response.status).to.equal(200);
          expect(response.body.policy.id).to.equal(policyID);
          expect(response.body.policy.name).to.equal(policyName);
          expect(response.body.policy.members).to.deep.equal(['team:local:test']);
          expect(response.body.policy.statements).to.deep.equal([{
            effect: 'ALLOW',
            actions: ['test:svc:someaction', 'test:svc:otheraction'],
            resources: ['*'],
            role: '',
            projects: [project1.id]
          }]);
          expect(response.body.policy.projects).to.deep.equal([project1.id]);
        });
      });

      it('returns 404 if policy not found', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'GET',
          url: '/apis/iam/v2/policies/not-a-real-policy-nope',
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.equal(404);
        });
      });
    });

    describe('PUT /apis/iam/v2/policies', () => {

      it('returns 200 when all valid inputs are provided', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'PUT',
          url: `/apis/iam/v2/policies/${policyID}`,
          body: {
            name: 'shiny new name',
            members: ['user:local:test2'],
            statements: [
              {
                effect: 'DENY',
                actions: ['test:svc:anotheraction'],
                projects: [project2.id]
              }
            ],
            projects: [project2.id]
          }
        }).then((response) => {
          expect(response.status).to.equal(200);
          expect(response.body.policy.id).to.equal(policyID);
          expect(response.body.policy.name).to.equal('shiny new name');
          expect(response.body.policy.members).to.deep.equal(['user:local:test2']);
          expect(response.body.policy.statements).to.deep.equal([{
            effect: 'DENY',
            actions: ['test:svc:anotheraction'],
            resources: ['*'],
            role: '',
            projects: [project2.id]
          }]);
          expect(response.body.policy.projects).to.deep.equal([project2.id]);
        });
      });

      it('returns 400 when there are no statements', () => {
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

    describe('DELETE /apis/iam/v2/policies/{policy_id}', () => {

      it('returns 200 on successful delete', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'DELETE',
          url: `/apis/iam/v2/policies/${policyID}`
        }).then((response) => {
          expect(response.status).to.equal(200);
          expect(response.body).to.deep.equal({});
        });
      });

      it('returns 404 if policy not found', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'DELETE',
          url: `/apis/iam/v2/policies/${policyID}`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.equal(404);
        });
      });

      it('returns 403 if policy is chef-managed', () => {
        cy.request({
          ...defaultAdminReq,
          method: 'DELETE',
          url: `/apis/iam/v2/policies/${chefManagedPolicyIDs[0]}`,
          failOnStatusCode: false
        }).then((response) => {
          expect(response.status).to.equal(403);
        });
      });
    });
  });

  describe('project assignment enforcement', () => {
    let nonAdminToken = '';
    const nonAdminTokenID = `${cypressPrefix}-nonadmin-token-${now}`;
    const statementProjects = [project1.id];
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
