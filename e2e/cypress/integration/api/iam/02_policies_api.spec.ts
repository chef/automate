// This is to prevent tests from running in wrong
// CI environments. If local, just always run it.
let iamVersion = <string><Object>Cypress.env('IAM_VERSION');
if (iamVersion === undefined) {
  iamVersion = 'v2.0';
}
const describeIfIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;

describeIfIAMV2('policies API', () => {
  let adminToken = '';
  const cypressPrefix = 'test-policies-api';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const projectID = `${cypressPrefix}-project-${now}`;

  beforeEach(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;
      cy.cleanupPoliciesByIDPrefix(adminToken, cypressPrefix);
      cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);

      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/apis/iam/v2beta/projects',
        body: {
          id: projectID,
          name: `${cypressPrefix} project ${now}`
        }
      });
    });
  });

  afterEach(() => {
    cy.cleanupPoliciesByIDPrefix(adminToken, cypressPrefix);
    cy.cleanupProjectsByIDPrefix(adminToken, cypressPrefix);
  });

  describe('POST /apis/iam/v2beta/policies', () => {
    it('returns 400 when there are no statements',  () => {
      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/apis/iam/v2beta/policies',
        failOnStatusCode: false,
        body: {
          id: `${cypressPrefix}-policy-${now}`,
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          projects: [projectID]
        }
      }).then((response) => {
       expect(response.status).to.equal(400);
      });
    });
  });

  describe('PUT /apis/iam/v2beta/policies', () => {
    const policyID = `${cypressPrefix}-policy-${now}`;
    beforeEach(() => {
      cy.request({
        auth: { bearer: adminToken },
        method: 'POST',
        url: '/apis/iam/v2beta/policies',
        body: {
          id: policyID,
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          statements: [
            {
              effect: 'ALLOW',
              actions: ['test:svc:someaction', 'test:svc:otheraction'],
              projects: [projectID]
            }
          ],
          projects: [projectID]
        }
      });
    });

    it('returns 400 when there are no statements',  () => {
      cy.request({
        auth: { bearer: adminToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/policies/${policyID}`,
        failOnStatusCode: false,
        body: {
          name: `${cypressPrefix} policy ${now}`,
          members: ['team:local:test'],
          projects: [projectID]
        }
      }).then((response) => {
       expect(response.status).to.equal(400);
      });
    });
  });
});
