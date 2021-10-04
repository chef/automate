describe('Infra servers post api to create infra servers', () => {
    let withInfraServersPostActionToken = '';
    let withoutInfraServersPostActionToken = '';

    const cypressPrefix = 'infra-server-cookbooks-actions-list';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersPostPolicy = {
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


    const withoutInfraServersPostPolicy = {
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
                withInfraServersPostActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersPostPolicy
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
                withoutInfraServersPostActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersPostPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });

        it('infra servers cookbooks get returns 200 when infraServersOrgsCookbooks list actions is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersPostActionToken },
                method: 'GET',
                url: `/api/v0/infra/servers/${server_id}/orgs/${org_id}/cookbooks`,
                }).then((resp) => {
                    assert.equal(resp.status, 200);
                });
        });

        it('infra servers post returns 403 when infraServers create actions is deneyed', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersPostActionToken },
                method: 'POST',
                url: '/api/v0/infra/servers',
                failOnStatusCode: false,
                body: {
                    fqdn: 'a2-dev.test',
                    id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                    ip_address: '127.0.0.1',
                    name: 'test'
                }
                }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });
    });