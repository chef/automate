const serverID = `chef-server-dev-test-${Cypress.moment().format('MMDDYYhhmm')}`;
const serverName = 'chef server dev';
const orgID = `chef-org-dev-${Cypress.moment().format('MMDDYYhhmm')}`;
const orgName = '4thcoffee';
const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
const serverIP = '34.219.25.251';
const adminUser = 'chefadmin';
// using dummy admin key value for creating the org
const adminKey = 'Dummy--admin--key';

describe('chef orgs list', () => {
    let withInfraServersOrgsListActionToken = '';
    let withoutInfraServersOrgsListActionToken = '';

    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-list';
    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsListPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgs:list'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersOrgsListPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgs:list'
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
                withInfraServersOrgsListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersOrgsListPolicy
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
            withoutInfraServersOrgsListActionToken = resp.body.token.value;
          });

          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersOrgsListPolicy
          }).then((resp) => {
            expect(resp.status).to.equal(200);
          });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/api/v0/infra/servers',
            body: {
              id: serverID,
              name: serverName,
              fqdn: serverFQDN,
              ip_address: serverIP
            }
          });

        cy.request({
          headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
          method: 'POST',
          url: `/api/v0/infra/servers/${serverID}/orgs`,
          body: {
            id: orgID,
            server_id: serverID,
            name: orgName,
            admin_user: adminUser,
            admin_key: adminKey
          }
        });
    });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
      });
it('infra server orgs list returns 200 when infraServers orgs list action is allowed', () => {
    cy.request({
        headers: { 'api-token': withInfraServersOrgsListActionToken,
        'content-type': 'application/json+lax' },
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs`
    }).then((resp) => {
        assert.equal(resp.status, 200);
        });
    });

    it('infra server orgs list returns 403 when infraServers orgs list actions is deneyed', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersOrgsListActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            failOnStatusCode: false,
            url: `/api/v0/infra/servers/${serverID}/orgs`
        }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('chef orgs get', () => {
    let withInfraServersOrgsGetActionToken = '';
    let withoutInfraServersOrgsGetActionToken = '';
    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-get';
    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsGetPolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgs:get'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersOrgsGetPolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgs:get'
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
            withInfraServersOrgsGetActionToken = resp.body.token.value;
        });

    cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/policies',
        body: withInfraServersOrgsGetPolicy
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
            withoutInfraServersOrgsGetActionToken = resp.body.token.value;
        });

        cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/policies',
        body: withoutInfraServersOrgsGetPolicy
        }).then((resp) => {
            expect(resp.status).to.equal(200);
        });
});

after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

    it('infra server orgs get returns 200 when infraServers orgs get action is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersOrgsGetActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}`
        }).then((resp) => {
            assert.equal(resp.status, 200);
            });
        });

    it('infra server orgs get returns 403 when infraServers orgs get actions is deneyed', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersOrgsGetActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            failOnStatusCode: false,
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}`
        }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

describe('chef orgs create', () => {
    let withInfraServersOrgsCreateActionToken = '';
    let withoutInfraServersOrgsCreateActionToken = '';
    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-create';
    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsCreatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgs:create',
                'iam:projects:assign'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersOrgsCreatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgs:create',
                'iam:projects:assign'
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
                withInfraServersOrgsCreateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersOrgsCreatePolicy
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
                withoutInfraServersOrgsCreateActionToken = resp.body.token.value;
            });

            cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersOrgsCreatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
    });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });

it('infra server orgs post returns 200 when infraServers orgs create action is allowed', () => {
    cy.request({
        headers: { 'api-token': withInfraServersOrgsCreateActionToken,
        'content-type': 'application/json+lax' },
        method: 'POST',
        body: {
            id: `${orgID}-test`,
            server_id: serverID,
            name: orgName,
            admin_user: adminUser,
            admin_key: adminKey
        },
        url: `/api/v0/infra/servers/${serverID}/orgs`
    }).then((resp) => {
        assert.equal(resp.status, 200);
        });
    });

it('infra server orgs post returns 403 when infraServers orgs create actions is deneyed', () => {
    cy.request({
        headers: { 'api-token': withoutInfraServersOrgsCreateActionToken,
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

describe('chef orgs update', () => {
    let withInfraServersOrgsUpdateActionToken = '';
    let withoutInfraServersOrgsUpdateActionToken = '';
    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-update';
    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsUpdatePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgs:update'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersOrgsUpdatePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgs:update'
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
                withInfraServersOrgsUpdateActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersOrgsUpdatePolicy
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
                withoutInfraServersOrgsUpdateActionToken = resp.body.token.value;
            });

            cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersOrgsUpdatePolicy
            }).then((resp) => {
                expect(resp.status).to.equal(200);
            });
    });

    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
        });

it('infra server orgs put returns 200 when infraServers orgs update action is allowed', () => {
    cy.request({
        headers: { 'api-token': withInfraServersOrgsUpdateActionToken,
        'content-type': 'application/json+lax' },
        method: 'PUT',
        body: {
            server_id: serverID,
            name: orgName,
            admin_user: adminUser,
            admin_key: adminKey
        },
        url: `/api/v0/infra/servers/${serverID}/orgs/${`${orgID}-test`}`
    }).then((resp) => {
        assert.equal(resp.status, 200);
        });
    });

it('infra server orgs put returns 403 when infraServers orgs update actions is deneyed', () => {
    cy.request({
        headers: { 'api-token': withoutInfraServersOrgsUpdateActionToken,
        'content-type': 'application/json+lax' },
        method: 'PUT',
        body: {
            server_id: serverID,
            name: orgName,
            admin_user: adminUser,
            admin_key: adminKey
        },
        failOnStatusCode: false,
        url: `/api/v0/infra/servers/${serverID}/orgs/${`${orgID}-test-1`}`
    }).then((resp) => {
        assert.equal(resp.status, 403);
    });
});
});

describe('chef orgs delete', () => {
    let withInfraServersOrgsDeleteActionToken = '';
    let withoutInfraServersOrgsDeleteActionToken = '';
    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-delete';
    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsDeletePolicy = {
    id: policyId1,
    name: tokenId1,
    projects: [],
    members: [`token:${tokenId1}`],
    statements: [
        {
            effect: 'ALLOW',
            actions: [
                'infra:infraServersOrgs:delete'
            ],
            projects: ['*']
        }]
    };


    const withoutInfraServersOrgsDeletePolicy = {
        id: policyId2,
        name: tokenId2,
        projects: [],
        members: [`token:${tokenId2}`],
        statements: [
        {
            effect: 'DENY',
            actions: [
                'infra:infraServersOrgs:delete'
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
            withInfraServersOrgsDeleteActionToken = resp.body.token.value;
        });

    cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/policies',
        body: withInfraServersOrgsDeletePolicy
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
            withoutInfraServersOrgsDeleteActionToken = resp.body.token.value;
        });

        cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: '/apis/iam/v2/policies',
        body: withoutInfraServersOrgsDeletePolicy
        }).then((resp) => {
            expect(resp.status).to.equal(200);
        });
});

after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
    });

it('infra server orgs delete returns 200 when infraServers orgs delete action is allowed', () => {
    cy.request({
        headers: { 'api-token': withInfraServersOrgsDeleteActionToken,
        'content-type': 'application/json+lax' },
        method: 'DELETE',
        url: `/api/v0/infra/servers/${serverID}/orgs/${`${orgID}-test`}`
    }).then((resp) => {
        assert.equal(resp.status, 200);
        });
    });

it('infra server orgs delete returns 403 when infraServers orgs delete actions is deneyed', () => {
    cy.request({
        headers: { 'api-token': withoutInfraServersOrgsDeleteActionToken,
        'content-type': 'application/json+lax' },
        method: 'DELETE',
        failOnStatusCode: false,
        url: `/api/v0/infra/servers/${serverID}/orgs/${`${orgID}-test-1`}`
    }).then((resp) => {
        assert.equal(resp.status, 403);
    });
});
});
