describe('Infra Viewer Policy', () => {
    let withInfraViewerActionToken = '';
    let withoutInfraViewerActionToken = '';

    const cypressPrefix = 'infra-viewer';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const allowInfraViewerPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'secrets:*:get',
                'secrets:*:list',
                'infra:*:get',
                'infra:*:list',
                'compliance:*:get',
                'compliance:*:list',
                'event:*:get',
                'event:*:list',
                'ingest:*:get',
                'ingest:*:list',
                'iam:projects:list',
                'iam:projects:get',
                'applications:*:get',
                'applications:*:list'
            ],
            projects: ['*']
        }]
    };


    const denyInfraViewerPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'secrets:*:get',
                'secrets:*:list',
                'infra:*:get',
                'infra:*:list',
                'compliance:*:get',
                'compliance:*:list',
                'event:*:get',
                'event:*:list',
                'ingest:*:get',
                'ingest:*:list',
                'iam:projects:list',
                'iam:projects:get',
                'applications:*:get',
                'applications:*:list'
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
                withInfraViewerActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: allowInfraViewerPolicy
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
                withoutInfraViewerActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: denyInfraViewerPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('cookbooks get returns 200 when infraViewer policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraViewerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when infraViewer policy actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraViewerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/cookbooks',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Create Env request returns 403 when infraViewer policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraViewerActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'test',
                description: 'cypress testing'
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/environments',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Roles get returns 200 when infraViewer policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraViewerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('Roles get returns 403 when infraViewer policy is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraViewerActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/roles',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('databags delete returns 403 when infraViewer policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraViewerActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/test',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });

    it('Nodes put returns 403 when infraViewer policy is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraViewerActionToken },
            method: 'PUT',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/nodes/test-admin',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
