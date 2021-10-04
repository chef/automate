const serverID = `chef-server-dev-test-${Cypress.moment().format('MMDDYYhhmm')}`;
const serverName = 'chef server dev';
const orgID = `chef-org-dev-${Cypress.moment().format('MMDDYYhhmm')}`;
const orgName = '4thcoffee';
const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
const serverIP = '34.219.25.251';
const adminUser = 'chefadmin';
// using dummy admin key value for creating the org
const adminKey = 'Dummy--admin--key';

describe('Infra servers post api to create infra servers', () => {
    let withInfraServersCookbooksListActionToken = '';
    let withoutInfraServersCookbooksListActionToken = '';

    const cypressPrefix = 'infra-server-cookbooks-actions-list';
    const policyId1 = `${cypressPrefix}-pol-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const policyId2 = `${cypressPrefix}-pol-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId1 = `${cypressPrefix}-token-1-${Cypress.moment().format('MMDDYYhhmm')}`;
    const tokenId2 = `${cypressPrefix}-token-2-${Cypress.moment().format('MMDDYYhhmm')}`;
    const objectsToCleanUp = ['tokens', 'policies'];

    const withInfraServersCookbooksListPolicy = {
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


    const withoutInfraServersCookbooksListPolicy = {
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
                withInfraServersCookbooksListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withInfraServersCookbooksListPolicy
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
                withoutInfraServersCookbooksListActionToken = resp.body.token.value;
            });

        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: withoutInfraServersCookbooksListPolicy
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

    it('cookbooks get returns 200 when infraServersOrgsCookbooks list actions is allowed', () => {
        cy.request({
            headers: { 'api-token': withInfraServersCookbooksListActionToken },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/cookbooks`
            }).then((resp) => {
                assert.equal(resp.status, 200);
            });
    });

    it('cookbooks get returns 403 when infraServersOrgsCookbooks list actions is deneyed', () => {
        cy.request({
            headers: { 'api-token': withoutInfraServersCookbooksListActionToken },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/cookbooks`,
            failOnStatusCode: false
            }).then((resp) => {
            assert.equal(resp.status, 403);
        });
    });
});
