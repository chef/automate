const serverID = `chef-server-dev-test-${Cypress.moment().format('MMDDYYhhmm')}`;
const serverName = 'chef server dev role';
const orgID = `chef-org-dev-${Cypress.moment().format('MMDDYYhhmm')}`;
const orgName = 'chef-server-role';
const serverFQDN = 'a2-dev.test';
const serverIP = '127.0.0.1';
const adminUser = 'chefadmin';
// using dummy admin key value for creating the org
const adminKey = 'Dummy--admin--key';

const now = Cypress.moment().format('MMDDYYhhmm');

const cypressPrefix = 'infra-server-orgs-actions-list';
const roleName = `${cypressPrefix}-role-${now}-1`;
const roleDescription = 'role description';
const roleRunlistName = `${cypressPrefix}-role-${now}-2`;
const roleDefaultAttrName = `${cypressPrefix}-role-${now}-3`;
const roleOverrideAttrName = `${cypressPrefix}-role-${now}-4`;

describe('infra server roles list', () => {
    let withInfraServersOrgsRolesListActionToken = '';
    let withoutInfraServersOrgsRolesListActionToken = '';

    const objectsToCleanUp = ['tokens', 'policies'];

    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsRolesListPolicy = {
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


    const withoutInfraServersOrgsRolesListPolicy = {
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
                withInfraServersOrgsRolesListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersOrgsRolesListPolicy
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
            withoutInfraServersOrgsRolesListActionToken = resp.body.token.value;
          });

          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersOrgsRolesListPolicy
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

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles`,
            body: {
              org_id: orgID,
              server_id: serverID,
              name: orgName,
              description: roleDescription,
              default_attributes: roleDefaultAttrName,
              override_attributes: roleOverrideAttrName,
              run_list: roleRunlistName,
            }
          });
    });



    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
      });

    it('infra server roles list returns 200 when infraServers roles list action is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersOrgsRolesListActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles`
        }).then((resp) => {
            assert.equal(resp.status, 200);
            });
        });

    it('infra server roles list returns 403 when infraServers roles list actions is deneyed', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersOrgsRolesListActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            failOnStatusCode: false,
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles`
        }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});


describe('infra server roles get', () => {
    let withInfraServersOrgsRolesGetActionToken = '';
    let withoutInfraServersOrgsRolesGetActionToken = '';

    const objectsToCleanUp = ['tokens', 'policies'];

    const policyId1 = `${cypressPrefix}-pol-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-orgs-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-orgs-2-${Cypress.moment().format('MMDDYYhhmm')}`;

    const withInfraServersOrgsRolesGetPolicy = {
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


    const withoutInfraServersOrgsRolesGetPolicy = {
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
                withInfraServersOrgsRolesGetActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersOrgsRolesGetPolicy
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
            withoutInfraServersOrgsRolesGetActionToken = resp.body.token.value;
          });

          cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersOrgsRolesGetPolicy
          }).then((resp) => {
            expect(resp.status).to.equal(200);
          });
    });



    after(() => {
        cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, objectsToCleanUp);
      });

    it('infra server get list returns 200 when infraServers roles get action is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersOrgsRolesGetActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}`
        }).then((resp) => {
            assert.equal(resp.status, 200);
            });
        });

    it('infra server roles get returns 403 when infraServers roles get actions is deneyed', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersOrgsRolesGetActionToken,
            'content-type': 'application/json+lax' },
            method: 'GET',
            failOnStatusCode: false,
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}`
        }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});

