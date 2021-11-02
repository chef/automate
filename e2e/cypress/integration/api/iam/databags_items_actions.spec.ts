describe('DataBags get', () => {
    let withInfraServersDataBagsGetActionToken = '';
    let withoutInfraServersDataBagsGetActionToken = '';

    const cypressPrefix = 'infra-server-databags-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBags:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBags:get'
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
                withInfraServersDataBagsGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsGetPolicy
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
                withoutInfraServersDataBagsGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databags get returns 200 when infraServersOrgsDataBags get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databags get returns 403 when infraServersOrgsDataBags get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});


describe('Create DataBags', () => {
    let withInfraServersDataBagsCreateActionToken = '';
    let withoutInfraServersDataBagsCreateActionToken = '';

    const cypressPrefix = 'infra-server-databags-actions-create';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsCreatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBags:create'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsCreatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBags:create'
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
                withInfraServersDataBagsCreateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsCreatePolicy
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
                withoutInfraServersDataBagsCreateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsCreatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databags post returns 200 when infraServersOrgsDataBags create actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsCreateActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'test'
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databags post returns 404 when infraServersOrgsDataBags create actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsCreateActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'test'
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data-bags',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 404);
        });
    });
});


describe('DataBagsItem get', () => {
    let withInfraServersDataBagsItemGetActionToken = '';
    let withoutInfraServersDataBagsItemGetActionToken = '';

    const cypressPrefix = 'infra-server-databagsitem-actions-get';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsItemGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBagsItem:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsItemGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBagsItem:get'
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
                withInfraServersDataBagsItemGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsItemGetPolicy
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
                withoutInfraServersDataBagsItemGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsItemGetPolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databagsitem get returns 200 when infraServersOrgsDataBagsItem get actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsItemGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databagsitem get returns 403 when infraServersOrgsDataBagsItem get actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsItemGetActionToken },
            method: 'GET',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Create DataBags item', () => {
    let withInfraServersDataBagsItemCreateActionToken = '';
    let withoutInfraServersDataBagsItemCreateActionToken = '';

    const cypressPrefix = 'infra-server-databags-item-actions-create';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsItemCreatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBagsItem:create'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsItemCreatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBagsItem:create'
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
                withInfraServersDataBagsItemCreateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsItemCreatePolicy
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
                withoutInfraServersDataBagsItemCreateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsItemCreatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databags post returns 200 when infraServersOrgsDataBagsItem create actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsItemCreateActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'colors',
                data: {id: "test1"}
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databags post returns 403 when infraServersOrgsDataBagsItem create actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsItemCreateActionToken },
            method: 'POST',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'colors',
                data: {id: "test1"}
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});


describe('Update DataBags item', () => {
    let withInfraServersDataBagsItemUpdateActionToken = '';
    let withoutInfraServersDataBagsItemUpdateActionToken = '';

    const cypressPrefix = 'infra-server-databags-item-actions-update';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsItemUpdatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBagsItem:update'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsItemUpdatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBagsItem:update'
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
                withInfraServersDataBagsItemUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsItemUpdatePolicy
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
                withoutInfraServersDataBagsItemUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsItemUpdatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databagsitem put returns 200 when infraServersOrgsDataBagsItem update actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsItemUpdateActionToken },
            method: 'PUT',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'colors',
                data: {id: "test1"}
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors/test1'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databagsitem put returns 403 when infraServersOrgsDataBagsItem update actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsItemUpdateActionToken },
            method: 'PUT',
            body: {
                org_id: 'test-org',
                server_id: 'local-dev',
                name: 'colors',
                data: {id: "test1"}
            },
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors/test1',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('Delete DataBagsItem', () => {
    let withInfraServersDataBagsItemDeleteActionToken = '';
    let withoutInfraServersDataBagsItemDeleteActionToken = '';

    const cypressPrefix = 'infra-server-databags-item-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsItemDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBagsItem:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsItemDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBagsItem:delete'
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
                withInfraServersDataBagsItemDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsItemDeletePolicy
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
                withoutInfraServersDataBagsItemDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsItemDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databagsitem delete returns 200 when infraServersOrgsDataBagsItem delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsItemDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors/test1'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databagsitem delete returns 403 when infraServersOrgsDataBags delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsItemDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/colors/test1',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});



describe('Delete DataBags', () => {
    let withInfraServersDataBagsDeleteActionToken = '';
    let withoutInfraServersDataBagsDeleteActionToken = '';

    const cypressPrefix = 'infra-server-databags-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersDataBagsDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgsDataBags:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersDataBagsDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgsDataBags:delete'
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
                withInfraServersDataBagsDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersDataBagsDeletePolicy
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
                withoutInfraServersDataBagsDeleteActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersDataBagsDeletePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
        });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('databags put returns 200 when infraServersOrgsDataBags delete actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersDataBagsDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/test'
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('databags put returns 403 when infraServersOrgsDataBags delete actions is denied', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersDataBagsDeleteActionToken },
            method: 'DELETE',
            url: '/api/v0/infra/servers/local-dev/orgs/test-org/data_bags/test',
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
