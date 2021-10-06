describe('Cookbooks list', () => {
    let withInfraServersCookbooksListActionToken = '';
    let withoutInfraServersCookbooksListActionToken = '';

    const cypressPrefix = 'infra-server-cookbooks-actions-list';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersCookbooksListPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsCookbooks:list'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersCookbooksListPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsCookbooks:list'
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
                withInfraServersCookbooksListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersCookbooksListPolicy
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
                withoutInfraServersCookbooksListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersCookbooksListPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('cookbooks get returns 200 when infraServersOrgsCookbooks list actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersCookbooksListActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when infraServersOrgsCookbooks list actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersCookbooksListActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Cookbooks get', () => {
    let withInfraServersCookbooksGetActionToken = '';
    let withoutInfraServersCookbooksGetActionToken = '';

    const cypressPrefix = 'infra-server-cookbooks-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersCookbooksGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsCookbooks:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersCookbooksGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsCookbooks:get'
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
                withInfraServersCookbooksGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersCookbooksGetPolicy
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
                withoutInfraServersCookbooksGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersCookbooksGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('cookbooks get returns 200 when infraServersOrgsCookbooks get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersCookbooksGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks/audit'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when infraServersOrgsCookbooks get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersCookbooksGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks/audit',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
