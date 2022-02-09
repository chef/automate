describe('Infra Editor Policy', () => {
    let withInfraEditorActionToken = '';
    let withoutInfraEditorActionToken = '';

    const cypressPrefix = 'infra-editor';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const allowInfraEditorPolicy = {
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
                'infra:infraServersOrgsClient:create',
                'infra:infraServersOrgsClient:update',
                'infra:infraServersOrgsDataBags:create',
                'infra:infraServersOrgsDataBagsItem:create',
                'infra:infraServersOrgsDataBagsItem:update',
                'infra:infraServersOrgsEnvironments:create',
                'infra:infraServersOrgsEnvironments:update',
                'infra:infraServersOrgsNodes:update',
                'compliance:*',
                'event:*',
                'ingest:*',
                'secrets:*',
                'iam:projects:list',
                'iam:projects:get',
                'iam:projects:assign',
                'applications:*'
            ],
            projects: ['*']
        }
        ]
    };


    const denyInfraEditorPolicy = {
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
                'infra:infraServersOrgsClient:create',
                'infra:infraServersOrgsClient:update',
                'infra:infraServersOrgsDataBags:create',
                'infra:infraServersOrgsDataBagsItem:create',
                'infra:infraServersOrgsDataBagsItem:update',
                'infra:infraServersOrgsEnvironments:create',
                'infra:infraServersOrgsEnvironments:update',
                'infra:infraServersOrgsNodes:update',
                'compliance:*',
                'event:*',
                'ingest:*',
                'secrets:*',
                'iam:projects:list',
                'iam:projects:get',
                'iam:projects:assign',
                'applications:*'
            ],
            projects: ['*']
        }
        ]
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
                withInfraEditorActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: allowInfraEditorPolicy
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
                withoutInfraEditorActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: denyInfraEditorPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('cookbooks get returns 200 when infraEditor policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when infraEditor policy actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraEditorActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Create Env request returns 200 when infraEditor policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'test2',
                description: 'cypress testing'
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/environments'
            }).then((resp) => {
            assert.equal(resp.status, 200);
        });
    });

    it('Roles get returns 200 when infraEditor policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Roles get returns 403 when infraEditor policy is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraEditorActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('databags delete returns 403 when infraEditor policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/test',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('create infra server post returns 403 when infraServers create actions is denied', () => {
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken },
            method: 'POST',
            url: '/api/v0/infra/servers',
            body: {
                fqdn: 'a2-dev.test',
                id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                ip_address: '127.0.0.1',
                name: 'test4'
            },
            failOnStatusCode: false
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
    });

    it('infra server orgs post returns 403 when infraServers orgs create actions is denied', () => {
        const serverID = `chef-server-dev-test-${Cypress.moment().format('MMDDYYhhmm')}`;
        const orgID = `chef-org-dev-${Cypress.moment().format('MMDDYYhhmm')}`;
        const orgName = '4thcoffee';
        const adminUser = 'chefadmin';
        // using dummy admin key value for creating the org
        const adminKey = 'Dummy--admin--key';
        cy.request({
            headers: { 'api-token': withInfraEditorActionToken,
            'content-type': 'application/json+lax' },
            method: 'POST',
            body: {
                id: `${orgID}-test-1`,
                server_id: serverID,
                name: orgName,
                admin_user: adminUser,
                admin_key: adminKey
            },
            failOnStatusCode: false,
            url: `/api/v0/infra/servers/${serverID}/orgs`
        }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
