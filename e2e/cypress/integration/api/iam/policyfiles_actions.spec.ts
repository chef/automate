describe('PolicyFiles get', () => {
    let withInfraServersPolicyFlesGetActionToken = '';
    let withoutInfraServersPolicyFlesGetActionToken = '';

    const cypressPrefix = 'infra-server-policyfiles-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersPolicyFilesPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsPolicyFiles:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersPolicyFilesPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsPolicyFiles:get'
            ],
            projects: ['*']
        }]
    };

    before(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/tokens',
            body: {
                id: tokenId1,
                name: tokenId1
            }
            }).then((resp) => {
                withInfraServersPolicyFlesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersPolicyFilesPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
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
                withoutInfraServersPolicyFlesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersPolicyFilesPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('policyfiles get returns 200 when get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersPolicyFlesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('policyfiles get returns 403 when get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersPolicyFlesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('PolicyFiles delete', () => {
    let withInfraServersPolicyFilesDeleteActionToken = '';
    let withoutInfraServersPolicyFilesDeleteActionToken = '';

    const cypressPrefix = 'infra-server-policyfiles-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersPolicyFilesDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsPolicyFiles:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersPolicyFilesDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsPolicyFiles:delete'
            ],
            projects: ['*']
        }]
    };

    before(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/tokens',
            body: {
                id: tokenId1,
                name: tokenId1
            }
            }).then((resp) => {
                withInfraServersPolicyFilesDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersPolicyFilesDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
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
                withoutInfraServersPolicyFilesDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersPolicyFilesDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('policyfiles delete returns 403 when delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersPolicyFilesDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles/examplecb',
            failOnStatusCode: false
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
    });

    it('policyfiles delete returns 200 when delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersPolicyFilesDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles/examplecb'
            }).then((resp) => {
            assert.equal(resp.status, 200);
        });
    });
});
