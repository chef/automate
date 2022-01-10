describe('Infra Project Owner Policy', () => {
    let withInfraProjectOwnerActionToken = '';
    let withoutInfraProjectOwnerActionToken = '';

    const cypressPrefix = 'infra-project-owner';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const allowInfraProjectOwnerPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:*:list',
                'infra:*:get',
                'infra:infraServersOrgsRoles:create',
                'infra:infraServersOrgsRoles:update',
                'infra:infraServersOrgsRoles:delete',
                'infra:infraServersOrgsClient:create',
                'infra:infraServersOrgsClient:update',
                'infra:infraServersOrgsClient:delete',
                'infra:infraServersOrgsDataBags:create',
                'infra:infraServersOrgsDataBags:delete',
                'infra:infraServersOrgsDataBagsItem:create',
                'infra:infraServersOrgsDataBagsItem:update',
                'infra:infraServersOrgsDataBagsItem:delete',
                'infra:infraServersOrgsEnvironments:create',
                'infra:infraServersOrgsEnvironments:update',
                'infra:infraServersOrgsEnvironments:delete',
                'infra:infraServersOrgsNodes:update',
                'infra:infraServersOrgsNodes:delete',
                'infra:infraServersOrgsPolicyFiles:delete',
                'compliance:*',
                'event:*',
                'ingest:*',
                'secrets:*',
                'iam:projects:list',
                'iam:projects:get',
                'iam:projects:assign',
                'iam:policies:list',
                'iam:policies:get',
                'iam:policyMembers:*',
                'iam:teams:list',
                'iam:teams:get',
                'iam:teamUsers:*',
                'iam:users:get',
                'iam:users:list',
                'applications:*'
            ],
            projects: ['*']
        }]
    };


    const denyInfraProjectOwnerPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:*:list',
                'infra:*:get',
                'infra:infraServersOrgsRoles:create',
                'infra:infraServersOrgsRoles:update',
                'infra:infraServersOrgsRoles:delete',
                'infra:infraServersOrgsClient:create',
                'infra:infraServersOrgsClient:update',
                'infra:infraServersOrgsClient:delete',
                'infra:infraServersOrgsDataBags:create',
                'infra:infraServersOrgsDataBags:delete',
                'infra:infraServersOrgsDataBagsItem:create',
                'infra:infraServersOrgsDataBagsItem:update',
                'infra:infraServersOrgsDataBagsItem:delete',
                'infra:infraServersOrgsEnvironments:create',
                'infra:infraServersOrgsEnvironments:update',
                'infra:infraServersOrgsEnvironments:delete',
                'infra:infraServersOrgsNodes:update',
                'infra:infraServersOrgsNodes:delete',
                'infra:infraServersOrgsPolicyFiles:delete',
                'compliance:*',
                'event:*',
                'ingest:*',
                'secrets:*',
                'iam:projects:list',
                'iam:projects:get',
                'iam:projects:assign',
                'iam:policies:list',
                'iam:policies:get',
                'iam:policyMembers:*',
                'iam:teams:list',
                'iam:teams:get',
                'iam:teamUsers:*',
                'iam:users:get',
                'iam:users:list',
                'applications:*'
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
                withInfraProjectOwnerActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: allowInfraProjectOwnerPolicy
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
                withoutInfraProjectOwnerActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: denyInfraProjectOwnerPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('cookbooks get returns 200 when InfraProjectOwner policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraProjectOwnerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when InfraProjectOwner policy actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraProjectOwnerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Create Env request returns 200 when InfraProjectOwner policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraProjectOwnerActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'test2',
                description: 'cypress testing'
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/environments',
            }).then((resp) => {
            assert.equal(resp.status, 200);
        });
    });

    it('Roles get returns 200 when InfraProjectOwner policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraProjectOwnerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Roles get returns 403 when InfraProjectOwner policy is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraProjectOwnerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Nodes delete returns 403 when infraServersOrgsNodes delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraProjectOwnerActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes/test-admin',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Nodes delete returns 200 when infraServersOrgsNodes delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraProjectOwnerActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes/test-admin'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('policyfiles delete returns 403 when delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraProjectOwnerActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles/examplecb',
            failOnStatusCode: false
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
    });

    it('policyfiles delete returns 200 when delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraProjectOwnerActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/policyfiles/examplecb'
            }).then((resp) => {
            assert.equal(resp.status, 200);
        });
    });
});