describe('chef orgs list', () => {
    let withInfraServersOrgsListActionToken = '';
    let withoutInfraServersOrgsListActionToken = '';
    const serverID = `chef-server-dev-test-${Cypress.moment().format('MMDDYYhhmm')}`;
    const serverName = 'chef server dev';
    const orgID = `chef-org-dev-${Cypress.moment().format('MMDDYYhhmm')}`;
    const orgName = '4thcoffee';
    const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
    const serverIP = '34.219.25.251';
    const adminUser = 'chefadmin';
    // using dummy admin key value for creating the org
    const adminKey = 'Dummy--admin--key';
    const objectsToCleanUp = ['tokens', 'policies'];

    const cypressPrefix = 'infra-server-orgs-actions-get';
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
                'infra:infraServers:list',
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
                'infra:infraServers:list',
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
          })

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
        })
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
    })