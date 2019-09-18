import { describeIfIAMV2p1 } from '../../constants';

describeIfIAMV2p1('assigning projects', () => {
  let adminIdToken = '';
  let apiToken = '';

  const modifyError = 'cannot modify a project for this object';

  // TODO iterate over these resources on each test pass
  // const resources = ['tokens', 'teams', 'policies', 'roles'];

  const cypressPrefix = 'iam-integration';
  const resource = {
    id: `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'test',
    projects: []
  };

  const notFoundProjectId = 'not-found-project';
  const authorizedProject1 = `${cypressPrefix}-authorized-project-1`;
  const authorizedProject2 = `${cypressPrefix}-authorized-project-2`;
  const unauthorizedProjectId = `${cypressPrefix}-unauthorized-project`;
  const policyId = `${cypressPrefix}-policy`;
  const tokenId = `${cypressPrefix}-token`;

  before(() => {
    cy.adminLogin('/').then(() => {
      adminIdToken = JSON.parse(<string>localStorage.getItem('chef-automate-user')).id_token;

      // TODO cleanup everything in resources + projects
      cy.cleanupV2IAMObjectsByIDPrefixes(adminIdToken, cypressPrefix, ['projects', 'tokens']);

      for (const id of [unauthorizedProjectId, authorizedProject1, authorizedProject2]) {
        cy.request({
          auth: { bearer: adminIdToken },
          method: 'POST',
          url: '/apis/iam/v2beta/projects',
          failOnStatusCode: false,
          body: {
            id: id,
            name: id
          }
        }).then((resp) => {
          expect([200, 409]).to.include(resp.status);
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
        failOnStatusCode: false,
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
                authorizedProject1,
                authorizedProject2
              ]
            }
          ]
        }
      }).then((resp) => {
        expect([200, 409]).to.include(resp.status);
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
        body: {id: resource.id, name: resource.name, projects: [authorizedProject1]}
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body.token.projects, [authorizedProject1]);
      });
    });

    it(`when updating ${token}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${token}/${resource.id}`,
        body: {
          name: `${resource.name} UpdatedName`,
          projects: [authorizedProject1, authorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body.token.projects, [authorizedProject1, authorizedProject2]);
      });
    });

    it(`when creating ${token}, fails to assign a non-existing project`, () => {
        cy.request({
          headers: { 'api-token': apiToken },
          method: 'POST',
          url: `/apis/iam/v2beta/${token}`,
          failOnStatusCode: false,
          body: { id: `${resource.id}-2`, name: resource.name, projects: [notFoundProjectId]}
        }).then((resp) => {
          assert.equal(resp.status, 404);
          expect(resp.body.error).to.have.string(modifyError);
        });
    });

    it(`when updating ${token}, fails to assign a non-existing project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${token}/${resource.id}`,
        failOnStatusCode: false,
        body: { name: `${resource.name} UpdatedName`, projects: [notFoundProjectId]}
      }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when creating ${token}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'POST',
        url: `/apis/iam/v2beta/${token}`,
        failOnStatusCode: false,
        body: { id: `${resource.id}-unauth`, name: resource.name, projects: [unauthorizedProjectId]}
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when updating ${token}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${token}/${resource.id}`,
        failOnStatusCode: false,
        body: {name: `${resource.name} UpdatedName`, projects: [unauthorizedProjectId]}
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });
  });
});
