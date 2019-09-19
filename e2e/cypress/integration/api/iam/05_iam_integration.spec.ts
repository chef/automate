import { describeIfIAMV2p1 } from '../../constants';

describeIfIAMV2p1('assigning projects', () => {
  let adminIdToken = '';
  let apiToken = '';

  const modifyError = 'cannot modify a project for this object';
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
  const iamResourcesToTest = ['roles', 'tokens', 'teams'];
  const objectsToCleanUp = iamResourcesToTest.concat(['projects']);

  before(() => {
    cy.adminLogin('/').then(() => {
      adminIdToken = JSON.parse(<string>localStorage.getItem('chef-automate-user')).id_token;

      // TODO cleanup everything in resources + projects
      cy.cleanupV2IAMObjectsByIDPrefixes(adminIdToken, cypressPrefix, objectsToCleanUp);

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
    cy.cleanupV2IAMObjectsByIDPrefixes(adminIdToken, cypressPrefix, objectsToCleanUp);
  });

  // iterate over iam resources
  iamResourcesToTest.forEach((iamResources: string) => {
    // get the singular form of the resource for json
    const iamResource = `${iamResources}`.slice(0, -1);

    it(`when creating ${iamResources}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'POST',
        url: `/apis/iam/v2beta/${iamResources}`,
        body: {
          id: resource.id,
          name: resource.name,
          projects: [authorizedProject1],
          actions: ['iam:users:create']
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body[`${iamResource}`]['projects'], [authorizedProject1]);
      });
    });

    it(`when updating ${iamResources}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${iamResources}/${resource.id}`,
        body: {
          name: `${resource.name} UpdatedName`,
          projects: [authorizedProject1, authorizedProject2],
          actions: ['iam:users:create']
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(
            resp.body[`${iamResource}`]['projects'],
            [authorizedProject1, authorizedProject2]);
      });
    });

    it(`when creating ${iamResources}, fails to assign a non-existing project`, () => {
        cy.request({
          headers: { 'api-token': apiToken },
          method: 'POST',
          url: `/apis/iam/v2beta/${iamResources}`,
          failOnStatusCode: false,
          body: {
            id: `${resource.id}-2`,
            name: resource.name,
            projects: [notFoundProjectId],
            actions: ['iam:users:create']
          }
        }).then((resp) => {
          assert.equal(resp.status, 404);
          expect(resp.body.error).to.have.string(modifyError);
        });
    });

    it(`when updating ${iamResources}, fails to assign a non-existing project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${iamResources}/${resource.id}`,
        failOnStatusCode: false,
        body: {
          name: `${resource.name} UpdatedName`,
          projects: [notFoundProjectId],
          actions: ['iam:users:create']
        }
          }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when creating ${iamResources}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'POST',
        url: `/apis/iam/v2beta/${iamResources}`,
        failOnStatusCode: false,
        body: {
          id: `${resource.id}-unauth`,
          name: resource.name,
          projects: [unauthorizedProjectId],
          actions: ['iam:users:create']
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when updating ${iamResources}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': apiToken },
        method: 'PUT',
        url: `/apis/iam/v2beta/${iamResources}/${resource.id}`,
        failOnStatusCode: false,
        body: {
          name: `${resource.name} UpdatedName`,
          projects: [unauthorizedProjectId],
          actions: ['iam:users:create']
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });
  });
});
