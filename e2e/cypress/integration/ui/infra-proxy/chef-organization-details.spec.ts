describe('chef server', () => {
  // let adminIdToken = '';
  // const now = Cypress.moment().format('MMDDYYhhmmss');
  // const cypressPrefix = 'infra';
  // const serverName = `${cypressPrefix} server ${now}`;
  // const serverID = serverName.split(' ').join('-');
  
  // const serverFQDN = 'chef-server-1617089723092818000.com';
  // const serverIP = '176.119.50.159';
  // const orgName =  `${cypressPrefix} org ${now}`;  
  // const orgID = orgName.split(' ').join('-');
  // const adminUser = 'test_admin_user';

  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  let adminIdToken = '';
  const serverID = 'chef-server-dev-test';
  const serverName = 'chef server dev';
  const orgID = 'chef-org-dev';
  const orgName = '4thcoffee';
  const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
  const serverIP = '34.219.25.251';
  const adminUser = 'chefadmin';
  const adminKey = 'Dummy--admin--key';

 
  const tabNames = ['Roles','Environments','Data Bags','Clients']

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;

      cy.request({
        auth: { bearer: adminIdToken },
        failOnStatusCode: false,
        method: 'POST',
        url: '/api/v0/infra/servers',
        body: {
          id: serverID,
          name: serverName,
          fqdn: serverFQDN,
          ip_address: serverIP
        }
      }).then((resp) => {
        if (resp.status === 200 && resp.body.ok === true) {
          return;
        } else {
          cy.request({
            auth: { bearer: adminIdToken },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}`,
            body: {
              id: serverID
            }
          });
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        failOnStatusCode: false,
        method: 'POST',
        url: `/api/v0/infra/servers/${serverID}/orgs`,
        body: {
          id: orgID,
          server_id: serverID,
          name: orgName,
          admin_user: adminUser,
          admin_key: adminKey
        }
      }).then((response) => {
        if (response.status === 200 && response.body.ok === true) {
          return;
        } else {
          cy.request({
            auth: { bearer: adminIdToken },
            method: 'GET',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}`,
            body: {
              id: orgID,
              server_id: serverID
            }
          });
        }
      });

      cy.visit(`/infrastructure/chef-servers/${serverID}/organizations/${orgID}`);
      cy.get('app-welcome-modal').invoke('hide');
    });

    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  describe('chef organizations details page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    it('on page load Cookbook is in active state',() => {
      cy.get('.nav-tab').contains('Cookbooks').should('have.class', 'active');
    });

    it('lists of Cookbook', () => {
      cy.get('.cookbooks').then(($cookbook) => {
        if($cookbook.hasClass('empty-section')) {
          cy.get('chef-table chef-th').should('not.be.visible');
            cy.get('.empty-section').should('be.visible');
            cy.get('.empty-section p').contains('No cookbooks available');
        } else {
          cy.get('chef-table chef-th').contains('Name');
          cy.get('chef-table chef-th').contains('Cookbook Version');
        }
      })     
    });

    tabNames.forEach((val) => {
    it(`can switch to ${val} tab`, () => {      
        cy.get('.nav-tab').contains(val).click();
      });      
    });   

  });
});
