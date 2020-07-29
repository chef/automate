describe('assigning projects', () => {
  let twoAllowedProjectsTok = '';
  let unassignedAndProjectAllowedTok = '';

  const modifyError = 'Projects cannot be modified';
  const cypressPrefix = 'iam-integration';
  const resource = {
    id: `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmm')}`,
    name: 'test',
    // TODO Roles api requires `actions`, so we're including it every time.
    // We should use a function (or a hash of resource: body)
    // For now, better to test it with some ugliness
    actions: ['iam:users:create']
  };
  const notFoundProjectId = 'not-found-project';
  const authorizedProject1 = `${cypressPrefix}-auth-proj-1`;
  const authorizedProject2 = `${cypressPrefix}-auth-proj-2`;
  const unauthorizedProject1 = `${cypressPrefix}-unauthd-proj`;
  const unauthorizedProject2 = `${cypressPrefix}-unauthd-proj`;
  const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
  const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
  const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
  const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
  const iamResourcesToTest = ['roles', 'tokens', 'teams'];
  const objectsToCleanUp = iamResourcesToTest.concat(['projects', 'policies']);

  before(() => {
    // TODO cleanup projects in before block (can't do now bc we have a project
    // limit and cereal runs async to delete projects)
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, iamResourcesToTest);

    for (const id of [
      authorizedProject1,
      authorizedProject2,
      unauthorizedProject1,
      unauthorizedProject2
    ]) {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/projects',
        failOnStatusCode: false,
        body: {
          id: id,
          name: id,
          skip_policies: true
        }
      }).then((resp) => {
        expect(resp.status).to.be.oneOf([200, 409]);
      });
    }

    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/tokens',
      body: {
        id: tokenId1,
        name: tokenId1
      }
    }).then((resp) => {
      twoAllowedProjectsTok = resp.body.token.value;
    });

    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/policies',
      failOnStatusCode: false,
      body: {
        id: policyId1,
        name: policyId1,
        members: [
          `token:${tokenId1}`
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
    });

    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/tokens',
      body: {
        id: tokenId2,
        name: tokenId2
      }
    }).then((resp) => {
      unassignedAndProjectAllowedTok = resp.body.token.value;
    });

    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: '/apis/iam/v2/policies',
      body: {
        id: policyId2,
        name: policyId2,
        members: [
          `token:${tokenId2}`
        ],
        statements: [
          {
            effect: 'ALLOW',
            actions: ['*'],
            projects: [
              authorizedProject1,
              '(unassigned)'
            ]
          }
        ]
      }
    });
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
  });

  iamResourcesToTest.forEach((iamResources: string) => {

    // get the singular form of the resource for json
    const iamResource = `${iamResources}`.slice(0, -1);

    it(`when creating ${iamResources}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        body: {
          ...resource,
          projects: [authorizedProject1]
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body[`${iamResource}`]['projects'], [authorizedProject1]);
      });
    });

    it(`when creating ${iamResources} without projects is allowed, ` +
    `successfully creates unassigned ${iamResource}`,  () => {
      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-no-projects`,
          projects: []
        }
      }).then((resp) => {
        expect(resp.status).to.equal(200);
        expect(resp.body[`${iamResource}`]['projects']).to.have.length(0);
      });
    });

    it(`when updating ${iamResources}, successfully assigns an existing authorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}`,
        body: {
          ...resource,
          projects: [authorizedProject1, authorizedProject2]
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
          headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
          method: 'POST',
          url: `/apis/iam/v2/${iamResources}`,
          failOnStatusCode: false,
          body: {
            ...resource,
            projects: [notFoundProjectId]
           }
        }).then((resp) => {
          assert.equal(resp.status, 404);
          expect(resp.body.error).to.have.string(modifyError);
        });
    });

    it(`when updating ${iamResources}, fails to assign a non-existing project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          projects: [notFoundProjectId]
        }
        }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when creating ${iamResources}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          projects: [unauthorizedProject1]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when creating ${iamResources} without any projects and ` +
    'project assignment permission is missing for (unassigned), ' +
    `fails to create unassigned ${iamResource}`,  () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          projects: []
        }
      }).then((resp) => {
          expect(resp.status).to.equal(403);
      });
    });

    it(`when updating ${iamResources}, fails to assign an unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          projects: [unauthorizedProject1]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when updating ${iamResources}, succeeds updating without changing projects`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}`,
        body: {
          ...resource,
          // existing projects (no change)
          projects: [authorizedProject1, authorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(
            resp.body[`${iamResource}`]['projects'],
            [authorizedProject1, authorizedProject2]);
      });
    });

    it(`when assigning one project and (unassigned) is allowed,
      succeeds creating (unassigned) ${iamResource}`, () => {
      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        body: {
          ...resource,
          id: `${resource.id}-2`,
          projects: []
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(
          resp.body[`${iamResource}`]['projects'], []);
      });
    });

    it(`when assigning one project and (unassigned) is allowed,
      succeeds updating ${iamResource} from (unassigned)
      to authorized project`, () => {
      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
         'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-2`,
        body: {
          ...resource,
          id: `${resource.id}-2`,
          projects: [authorizedProject1]
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(
          resp.body[`${iamResource}`]['projects'], [authorizedProject1]);
      });
    });

    it(`when assigning one project and (unassigned) is allowed,
      succeeds updating ${iamResource} from authorized project to (unassigned)`, () => {
      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-2`,
        body: {
          ...resource,
          id: `${resource.id}-2`,
          projects: []
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body[`${iamResource}`]['projects'], []);
      });
    });

    it(`when assigning one project and (unassigned) is allowed,
      fails to update ${iamResource} from unassigned to unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-2`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-2`,
          projects: [unauthorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed,
      fails to update ${iamResource} from (unassigned) to authorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-2`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-2`,
          projects: [authorizedProject1]
        }
      }).then((resp) => {
        // we get 404 instead of 403 because when the system tries to find the resource in the db
        // there is no intersection between twoAllowedProjectsTok's allowed projects
        // and the resource's current projects
        assert.equal(resp.status, 404);
      });
    });

    it(`when assigning only two projects is allowed,
      fails to create ${iamResource} with authorized and unauthorized projects`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-3`,
          projects: [authorizedProject1, unauthorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning one project and (unassigned) is allowed,
    succeeds updating ${iamResource} from authorized to (unassigned)`, () => {
        cy.request({
          headers: { 'api-token': unassignedAndProjectAllowedTok,
            'content-type': 'application/json+lax' },
          method: 'POST',
          url: `/apis/iam/v2/${iamResources}`,
          body: {
            ...resource,
            id: `${resource.id}-4a`,
            projects: [authorizedProject1]
          }
        });

      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-4a`,
        body: {
          ...resource,
          id: `${resource.id}-4a`,
          projects: []
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body[`${iamResource}`]['projects'], []);
      });
    });

    // special case to allow users to remove their authorized projects
    // even if they don't have access to (unassigned)
    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      succeeds removing authorized project from ${iamResource} to make it (unassigned)`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        body: {
          ...resource,
          id: `${resource.id}-4b`,
          projects: [authorizedProject1]
        }
      });

      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-4b`,
        body: {
          ...resource,
          id: `${resource.id}-4b`,
          projects: []
        }
      }).then((resp) => {
        assert.equal(resp.status, 200);
        assert.isUndefined(resp.body.error);
        assert.deepEqual(resp.body[`${iamResource}`]['projects'], []);
      });
    });

    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to update ${iamResource} from authorized to unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [authorizedProject1]
        }
      });

      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-5`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [unauthorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to update ${iamResource} from authorized to non-existent project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-5`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [notFoundProjectId]
        }
      }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to update ${iamResource} from authorized project
      to a different authorized project and non-existent project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-5`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [authorizedProject2, notFoundProjectId]
        }
      }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to update ${iamResource} from authorized project
      to an unauthorized project and non-existent project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-5`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [unauthorizedProject2, notFoundProjectId]
        }
      }).then((resp) => {
        assert.equal(resp.status, 404);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to update ${iamResource} from authorized project
      to a different authorized project and unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-5`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-5`,
          projects: [authorizedProject2, unauthorizedProject2]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    // TODO fails on teams only
    it(`when assigning only two projects is allowed and (unassigned) is not allowed,
      fails to create (unassigned) ${iamResource}`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-6`,
          projects: []
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    it(`when assigning only two projects is allowed,
    fails to update ${iamResource} to remove unauthorized project`, () => {
      cy.request({
        headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
        method: 'POST',
        url: `/apis/iam/v2/${iamResources}`,
        body: {
          ...resource,
          id: `${resource.id}-7`,
          projects: [authorizedProject1, authorizedProject2]
        }
      });

      cy.request({
        headers: { 'api-token': unassignedAndProjectAllowedTok,
          'content-type': 'application/json+lax' },
        method: 'PUT',
        url: `/apis/iam/v2/${iamResources}/${resource.id}-7`,
        failOnStatusCode: false,
        body: {
          ...resource,
          id: `${resource.id}-7`,
          // removes authorizedProject2, which is unauthorized to unassignedAndProjectAllowedTok
          projects: [authorizedProject1]
        }
      }).then((resp) => {
        assert.equal(resp.status, 403);
        expect(resp.body.error).to.have.string(modifyError);
      });
    });

    describe('when project assignment is not allowed but ' +
    `updating ${iamResource} is allowed`, () => {
      before(() => {
        // update seed policy
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'PUT',
          url: `/apis/iam/v2/policies/${policyId1}`,
          failOnStatusCode: false,
          body: {
            id: policyId1,
            name: policyId1,
            members: [
              `token:${tokenId1}` // aka twoAllowedProjectsTok
            ],
            statements: [
              {
                effect: 'ALLOW',
                actions: ['*'], // includes assigning projects
                projects: [
                  authorizedProject1,
                  authorizedProject2
                ]
              },
              {
                effect: 'ALLOW',
                actions: [`iam:${iamResources}:get`, `iam:${iamResources}:update`],
                projects: [ '*' ] // allows all projects only for the get and update action
              }
            ]
          }
        });
      });

      it('succeeds updating without changing project assignment when there are no projects', () => {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN'),
            'content-type': 'application/json+lax' },
          method: 'POST',
          url: `/apis/iam/v2/${iamResources}`,
          body: {
            ...resource,
            id: `${resource.id}-8`,
            projects: []
          }
        });

        cy.request({
          headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
          method: 'PUT',
          url: `/apis/iam/v2/${iamResources}/${resource.id}-8`,
          body: {
            ...resource,
            id: `${resource.id}-8`,
            name: `${resource.name} Updated Name`,
            projects: [] // projects unchanged
          }
        }).then((resp) => {
          assert.equal(resp.status, 200);
          assert.isUndefined(resp.body.error);
          assert.deepEqual(resp.body[`${iamResource}`]['projects'], []);
          assert.deepEqual(resp.body[`${iamResource}`]['name'], `${resource.name} Updated Name`);
        });
      });

      it('succeeds updating without changing project assignment ' +
      'when there there are unauthorizerd projects', () => {
        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN'),
            'content-type': 'application/json+lax' },
          method: 'POST',
          url: `/apis/iam/v2/${iamResources}`,
          body: {
            ...resource,
            id: `${resource.id}-9`,
            projects: [unauthorizedProject1]
          }
        });

        cy.request({
          headers: { 'api-token': twoAllowedProjectsTok, 'content-type': 'application/json+lax' },
          method: 'PUT',
          url: `/apis/iam/v2/${iamResources}/${resource.id}-9`,
          body: {
            ...resource,
            name: `${resource.name} Updated Name`,
            projects: [unauthorizedProject1] // projects unchanged
          }
        }).then((resp) => {
          assert.equal(resp.status, 200);
          assert.isUndefined(resp.body.error);
          assert.deepEqual(resp.body[`${iamResource}`]['projects'], [unauthorizedProject1]);
          assert.deepEqual(resp.body[`${iamResource}`]['name'], `${resource.name} Updated Name`);
        });
      });
    });
  });
});
