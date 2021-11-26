describe('Infra Clients get', () => {
    let withInfraServersClientGetActionToken = '';
    let withoutInfraServersClientGetActionToken = '';

    const cypressPrefix = 'infra-server-client-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersClientPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsClient:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersClientPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsClient:get'
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
                withInfraServersClientGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersClientPolicy
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
                withoutInfraServersClientGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersClientPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('client get returns 200 when get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersClientGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('clients get returns 403 when get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersClientGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Infra Clients create', () => {
    let withInfraServersClientCreateActionToken = '';
    let withoutInfraServersClientCreateActionToken = '';
    
        const cypressPrefix = 'infra-server-client-actions-create';
        const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
        const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
        const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
        const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
        const objectsToCleanUp = ['tokens', 'policies'];
    
        const withInfraServersClientCreatePolicy = {
        id: policyId1,
        name: tokenId1,
        projects: [],
        members: [`token:${tokenId1}`],
        statements: [
            {
                effect: 'ALLOW',
                actions: [
                    'infra:infraServersOrgsClient:create'
                ],
                projects: ['*']
            }]
        };
    
    
        const withoutInfraServersClientCreatePolicy = {
            id: policyId2,
            name: tokenId2,
            projects: [],
            members: [`token:${tokenId2}`],
            statements: [
            {
                effect: 'DENY',
                actions: [
                    'infra:infraServersOrgsClient:create'
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
                    withInfraServersClientCreateActionToken = resp.body.token.value;
                });
    
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/apis/iam/v2/policies',
                body: withInfraServersClientCreatePolicy
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
                    withoutInfraServersClientCreateActionToken = resp.body.token.value;
                });
    
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/apis/iam/v2/policies',
                body: withoutInfraServersClientCreatePolicy
                }).then((resp) => {
                    expect(resp.status).to.equal(200);
                });
            });
    
        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });
    
        it('client post returns 200 when create actions is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersClientCreateActionToken },
                method: 'POST',
                url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients',
                body: {
                    create_key: true,
                    name: 'test',
                    org_id: 'test-org',
                    server_id: 'local-dev',
                    validator: true
                },
                }).then((resp) => {
                    assert.equal(resp.status, 200);
                });
        });
    
        it('clients post returns 403 when create actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersClientCreateActionToken },
                method: 'POST',
                url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients',
                body: {
                    create_key: true,
                    name: 'test',
                    org_id: 'test-org',
                    server_id: 'local-dev',
                    validator: true
                },
                failOnStatusCode: false
                }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });
    });

    describe('Infra Reset Client Key', () => {
        let withInfraServersClientResetActionToken = '';
        let withoutInfraServersClientResetActionToken = '';
    
        const cypressPrefix = 'infra-server-client-actions-reset';
        const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
        const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
        const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
        const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
        const objectsToCleanUp = ['tokens', 'policies'];
    
        const withInfraServersClientResetPolicy = {
        id: policyId1,
        name: tokenId1,
        projects: [],
        members: [`token:${tokenId1}`],
        statements: [
            {
                effect: 'ALLOW',
                actions: [
                    'infra:infraServersOrgsClient:update'
                ],
                projects: ['*']
            }]
        };
    
    
        const withoutInfraServersClientResetPolicy = {
            id: policyId2,
            name: tokenId2,
            projects: [],
            members: [`token:${tokenId2}`],
            statements: [
            {
                effect: 'DENY',
                actions: [
                    'infra:infraServersOrgsClient:update'
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
                    withInfraServersClientResetActionToken = resp.body.token.value;
                });
    
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/apis/iam/v2/policies',
                body: withInfraServersClientResetPolicy
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
                    withoutInfraServersClientResetActionToken = resp.body.token.value;
                });
    
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/apis/iam/v2/policies',
                body: withoutInfraServersClientResetPolicy
                }).then((resp) => {
                    expect(resp.status).to.equal(200);
                });
            });
    
        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });
    
        it('client put returns 403 when update actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersClientResetActionToken },
                method: 'PUT',
                url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients/test/reset',
                failOnStatusCode: false
                }).then((resp) => {
                    assert.equal(resp.status, 403);
                });
        });
    
        it('client put returns 200 when update actions is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersClientResetActionToken },
                method: 'PUT',
                url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients/test/reset'
                }).then((resp) => {
                assert.equal(resp.status, 200);
            });
        });
    });

describe('Infra Client delete', () => {
    let withInfraServersClientDeleteActionToken = '';
    let withoutInfraServersClientDeleteActionToken = '';

    const cypressPrefix = 'infra-server-client-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersClientDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsClient:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersClientDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsClient:delete'
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
                withInfraServersClientDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersClientDeletePolicy
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
                withoutInfraServersClientDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersClientDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('client delete returns 403 when delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersClientDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients/test',
            failOnStatusCode: false
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
    });

    it('client delete returns 200 when delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersClientDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/clients/test'
            }).then((resp) => {
            assert.equal(resp.status, 200);
        });
    });
});
