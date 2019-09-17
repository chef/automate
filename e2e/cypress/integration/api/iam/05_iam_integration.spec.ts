import { describeIfIAMV2p1 } from '../../constants';

describeIfIAMV2p1('assigning projects', () => {
  let adminIdToken = '';
  let apiToken = '';

  // TODO iterate over these resources on each test pass
  // const resources = ['tokens', 'teams', 'policies', 'roles'];

  const cypressPrefix = 'iam-integration';
  const resource = {
    id: `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'test',
    projects: []
  };

  const notFoundProjectId = 'not-found-project';
  const authorizedProjectId = `${cypressPrefix}-authorized-project`;
  const unauthorizedProjectId = `${cypressPrefix}-unauthorized-project`;
  const policyId = `${cypressPrefix}-policy`;
  const tokenId = `${cypressPrefix}-token`;

  before(() => {
    cy.adminLogin('/').then(() => {
      adminIdToken = JSON.parse(<string>localStorage.getItem('chef-automate-user')).id_token;

      // TODO cleanup everything in resources + projects
      cy.cleanupV2IAMObjectsByIDPrefixes(adminIdToken, cypressPrefix, ['projects', 'tokens']);

      for (const id of [unauthorizedProjectId, authorizedProjectId]) {
        cy.request({
          auth: { bearer: adminIdToken },
          method: 'POST',
          url: '/apis/iam/v2beta/projects',
          body: {
            id: id,
            name: id
          }
        });
      }

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2beta/tokens',
        body: {
          id: tokenId,
          name: tokenId
        }
      }).then((resp) => {
        apiToken = resp.body.token.value;
      });

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/apis/iam/v2beta/policies',
        body: {
          id: policyId,
          name: policyId,
          members: [
            `token:${tokenId}`
          ],
          statements: [
            {
              effect: 'ALLOW',
              actions: ['*'],
              projects: [
                authorizedProjectId
              ]
            }
          ]
        }
      });
    });
  });

  after(() => {
    // TODO cleanup everything in resources + projects
    cy.cleanupV2IAMObjectsByIDPrefixes(adminIdToken, cypressPrefix, ['projects', 'tokens']);
  });

  // iterate over resources
  ['tokens'].forEach((token: string) => {
    it(`when creating ${token}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'POST',
        url: `/apis/iam/v2beta/${token}`,
        body: {id: `${token}-id`, name: `${token}Name`, projects: [authorizedProjectId]}
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.equal(resp.body.token.projects, [authorizedProjectId]);
      });
    });

    it(`when updating ${token}, successfully modifies membership for an existing
        authorized project`, () => {
        // deleting authorized project
    cy.request({
      headers: { 'api-token': apiToken },
      method: 'PUT',
      url: `/apis/iam/v2beta/${token}/${token}-id`,
      body: {name: `${token}UpdatedName`, projects: []}
    }).then((resp) => {
      assert.equal(resp.status, 200);
      assert.isUndefined(resp.body.error);
      assert.equal(resp.body.token.projects, []);
    });
    });

    it(`when creating ${token}, fails to assign a non-existing project`, () => {
        cy.request({
          headers: { 'api-token': apiToken },
          method: 'POST',
          url: `/apis/iam/v2beta/${token}`,
          body: {id: `${token}-2-id`, name: `${token}Name`, projects: [notFoundProjectId]}
        }).then((resp) => {
          assert.equal(resp.status, 404);
          assert.exists(resp.body.error);
          assert.isUndefined(resp.body);
        });
    });

    it(`when updating ${token}, fails to assign a non-existing project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${token}/${token}-id`,
        body: {name: `${token}Name`, projects: [unauthorizedProjectId]}
      }).then((resp) => {
          assert.equal(resp.status, 404);
          assert.exists(resp.body.error);
          assert.isUndefined(resp.body);
      });
    });

    it(`when creating ${token}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'POST',
        url: `/apis/iam/v2beta/${token}`,
        body: {id: `${token}-unauth-id`, name: `${token}Name`, projects: [unauthorizedProjectId]}
      }).then((resp) => {
          assert.equal(resp.status, 403);
          assert.exists(resp.body.error);
          assert.isUndefined(resp.body);
      });
    });

    it(`when updating ${token}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${token}/${token}-id`,
        body: {name: `${token}Name`, projects: [unauthorizedProjectId]}
      }).then((resp) => {
          assert.equal(resp.status, 403);
          assert.exists(resp.body.error);
          assert.isUndefined(resp.body);
      });
    });
  });
});
