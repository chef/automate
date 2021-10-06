describe('Roles list', () => {
    let withInfraServersRolesListActionToken = '';
    let withoutInfraServersRolesListActionToken = '';

    const cypressPrefix = 'infra-server-roles-actions-list';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersRolesListPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsRoles:list'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersRolesListPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsRoles:list'
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
                withInfraServersRolesListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersRolesListPolicy
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
                withoutInfraServersRolesListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersRolesListPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('Roles get returns 200 when infraServersOrgsRoles list actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersRolesListActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Roles get returns 403 when infraServersOrgsRoles list actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersRolesListActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Roles get', () => {
    let withInfraServersRolesGetActionToken = '';
    let withoutInfraServersRolesGetActionToken = '';

    const cypressPrefix = 'infra-server-roles-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersRolesGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsRoles:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersRolesGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsRoles:get'
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
                withInfraServersRolesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersRolesGetPolicy
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
                withoutInfraServersRolesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersRolesGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('roles get returns 200 when infraServersOrgsRoles get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersRolesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles/web'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('roles get returns 403 when infraServersOrgsRoles get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersRolesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles/web',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
