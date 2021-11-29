describe('Node get', () => {
    let withInfraServersNodesGetActionToken = '';
    let withoutInfraServersNodesGetActionToken = '';

    const cypressPrefix = 'infra-server-nodes-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersNodesGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsNodes:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersNodesGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsNodes:get'
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
                withInfraServersNodesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersNodesGetPolicy
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
                withoutInfraServersNodesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersNodesGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('Nodes get returns 200 when infraServersOrgsNodes get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersNodesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Nodes get returns 403 when infraServersOrgsNodes get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersNodesGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Nodes update', () => {
    let withInfraServersNodesUpdateActionToken = '';
    let withoutInfraServersNodesUpdateActionToken = '';

    const cypressPrefix = 'infra-server-nodes-actions-update';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersNodesUpdatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsNodes:update'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersNodesUpdatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsNodes:update'
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
                withInfraServersNodesUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersNodesUpdatePolicy
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
                withoutInfraServersNodesUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersNodesUpdatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('Nodes put returns 200 when infraServersOrgsNodes update actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersNodesUpdateActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes/test-admin'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Nodes put returns 403 when infraServersOrgsNodes put actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersNodesUpdateActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes/test-admin',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
