// these tests are best read sequentially, as they share state.
describe('Infra servers list', () => {
    let withInfraServersListActionToken = '';
    let withoutInfraServersListActionToken = '';

    const cypressPrefix = 'infra-server-actions-list';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersListPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServers:list'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersListPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServers:list'
            ],
            projects: ['*']
        }]
    };

    before(() => {
        // TODO cleanup projects in before block (can't do now bc we have a project
        // limit and cereal runs async to delete policies)
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
            withInfraServersListActionToken = resp.body.token.value;
          });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersListPolicy
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
            withoutInfraServersListActionToken = resp.body.token.value;
          });


        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersListPolicy
          }).then((resp) => {
            expect(resp.status).to.equal(200);
          });
        });
        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
          });

        it('infra servers list returns 200 when infraServers list actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersListActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            url: '/api/v0/infra/servers'
        }).then((resp) => {
            assert.equal(resp.status, 200);
            });
        });

        it('infra servers list returns 403 when infraServers list actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersListActionToken,
                'content-type': 'application/json+lax' },
                method: 'GET',
                failOnStatusCode: false,
                url: '/api/v0/infra/servers'
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });
    });

describe('Infra servers get api', () => {
    let withInfraServersGetActionToken = '';
    let withoutInfraServersGetActionToken = '';
    let server = {
        fqdn: '',
        id: '',
        ip_address: '',
        name: ''
    };
    const cypressPrefix = 'infra-server-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServers:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServers:get'
            ],
            projects: ['*']
        }]
    };

    before(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/api/v0/infra/servers',
            body: {
                fqdn: 'a2-dev.test',
                id: `${cypressPrefix}-test`,
                ip_address: '127.0.0.1',
                name: 'test'
            }
            }).then((resp) => {
                server = resp.body.server;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/tokens',
            body: {
                id: tokenId1,
                name: tokenId1
            }
            }).then((resp) => {
                withInfraServersGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersGetPolicy
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
                withoutInfraServersGetActionToken = resp.body.token.value;
            });


        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });
        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });

        it('infra servers get returns 200 when infraServers get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersGetActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            url: `/api/v0/infra/servers/${server.id}`
        }).then((resp) => {
                assert.equal(resp.status, 200);
            });
        });

        it('infra servers get returns 403 when infraServers get actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersGetActionToken,
                'content-type': 'application/json+lax' },
                method: 'GET',
                failOnStatusCode: false,
                url: `/api/v0/infra/servers/${server.id}`
            }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });
    });

describe('Infra servers post api to create infra servers', () => {
    let withInfraServersPostActionToken = '';
    let withoutInfraServersPostActionToken = '';

    const cypressPrefix = 'infra-server-actions-post';
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
                'infra:infraServers:create'
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
                'infra:infraServers:create'
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

        it('infra servers post returns 200 when infraServers create actions is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersPostActionToken },
                method: 'POST',
                url: '/api/v0/infra/servers',
                body: {
                    fqdn: 'a2-dev.test',
                    id: `${cypressPrefix}-test`,
                    ip_address: '127.0.0.1',
                    name: 'test'
                }
                }).then((resp) => {
                    assert.equal(resp.status, 200);
                });
        });

        it('infra servers post returns 403 when infraServers create actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersPostActionToken },
                method: 'POST',
                url: '/api/v0/infra/servers',
                failOnStatusCode: false,
                body: {
                    fqdn: 'a2-dev.test',
                    id: `${cypressPrefix}-test`,
                    ip_address: '127.0.0.1',
                    name: 'test'
                }
                }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });
    });

describe('Infra servers put api to update infra server', () => {
    let withInfraServersUpdateActionToken = '';
    let withoutInfraServersUpdateActionToken = '';
    let server = {
        fqdn: '',
        id: '',
        ip_address: '',
        name: ''
    };
    const cypressPrefix = 'infra-server-actions-update';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];


    const withInfraServersUpdatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServers:update'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersUpdatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServers:update'
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
                withInfraServersUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersUpdatePolicy
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
                withoutInfraServersUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersUpdatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/api/v0/infra/servers',
            body: {
                fqdn: 'a2-dev.test',
                id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                ip_address: '127.0.0.1',
                name: 'test'
            }
            }).then((resp) => {
                server = resp.body.server;
            });
        });

        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });


        it('infra servers put returns 403 when infraServers update actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersUpdateActionToken },
                method: 'PUT',
                url: `/api/v0/infra/servers/${server.id}`,
                failOnStatusCode: false
                }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });

        it('infra servers put returns 200 when infraServers update actions is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersUpdateActionToken },
                method: 'PUT',
                url: `/api/v0/infra/servers/${server.id}`,
                body: {
                    fqdn: 'a2-dev.test',
                    id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                    ip_address: '127.0.0.1',
                    name: 'test'
                }
                }).then((resp) => {
                    assert.equal(resp.status, 200);
                });
        });
    });

describe('Infra servers delete api to delete infra server', () => {
    let withInfraServersDeleteActionToken = '';
    let withoutInfraServersDeleteActionToken = '';
    let server = {
        fqdn: '',
        id: '',
        ip_address: '',
        name: ''
    };
    const cypressPrefix = 'infra-server-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];


    const withInfraServersDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServers:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServers:delete'
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
                withInfraServersDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDeletePolicy
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
                withoutInfraServersDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/api/v0/infra/servers',
            body: {
                fqdn: 'a2-dev.test',
                id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                ip_address: '127.0.0.1',
                name: 'test'
            }
            }).then((resp) => {
                server = resp.body.server;
            });
        });

        after(() => {
            cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });


        it('infra servers delete returns 403 when infraServer delete actions is denied', () => {
            cy.request({
                headers: { 'api-token': withoutInfraServersDeleteActionToken },
                method: 'DELETE',
                url: `/api/v0/infra/servers/${server.id}`,
                failOnStatusCode: false
                }).then((resp) => {
                assert.equal(resp.status, 403);
            });
        });

        it('infra server delete returns 200 when infraServer delete action is allowed', () => {
            cy.request({
                headers: { 'api-token': withInfraServersDeleteActionToken },
                method: 'DELETE',
                url: `/api/v0/infra/servers/${server.id}`,
                body: {
                    fqdn: 'a2-dev.test',
                    id: `${cypressPrefix}-test-${Cypress.moment().format('MMDDYYhhmm')}`,
                    ip_address: '127.0.0.1',
                    name: 'test'
                }
                }).then((resp) => {
                    assert.equal(resp.status, 200);
                });
        });
    });
