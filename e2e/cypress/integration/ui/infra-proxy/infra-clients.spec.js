describe('infra clients', () => {
  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  let adminIdToken = '';
  const serverID = 'chef-server-dev-test';
  const serverName = 'chef server dev';
  const orgID = 'demoorg';
  const orgName = 'demoorg';
  const serverFQDN = 'https://ec2-18-117-112-129.us-east-2.compute.amazonaws.com';
  const serverIP = '18-117-112-129';
  const adminUser = 'kallol';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const webuiKey = Cypress.env('AUTOMATE_INFRA_WEBUI_KEY').replace(/\\n/g, '\n');
  const clientName = `${cypressPrefix}-clients-${now}-1`;

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(localStorage.getItem('chef-automate-user'));
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
            ip_address: serverIP,
            webui_key: webuiKey
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
          auth: {bearer: adminIdToken },
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

    function getClients() {
      return cy.request({
        auth: {bearer: adminIdToken },
        failOnStatusCode: false,
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/clients`
      });
    }
    function checkResponse(response) {
      if (response.body.clients === 0) {
        cy.get('[data-cy=empty-list]').should('be.visible');
      } else {
        cy.get('[data-cy=create-client-button]').contains('Create Client');
        cy.get('[data-cy=clients-table-container] chef-th').contains('Name');
        return true;
      }
    }
  
    describe('infra clients list page', () => {
      it('displays org details', () => {
        cy.get('.page-title').contains(orgName);
      });

      // databags tabs specs
      it('can switch to Clients tab', () => {
          cy.get('.nav-tab').contains('Clients').click();
      });
  
      it('check if Clients tab has a list or not', () => {
        getClients().then((response) => {
          checkResponse(response);
        });
      });
    });
  
    context('can search and change page in Clients Tab', () => {
      it('can search a Client list and check if empty or not', () => {
        cy.get('[data-cy=search-filter]').type(clientName);
        cy.get('[data-cy=search-entity]').click();
        cy.get('[data-cy=clients-table-container]').then(body => {
          if (body.text().includes(clientName)) {
            cy.get('[data-cy=clients-table-container] [data-cy=select-run-list]')
              .contains(clientName).should('exist');
          }
        });
        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getClients().then((response) => {
          checkResponse(response);
        });
      });
    });
  
    context('can create client with details tab', () => {
      it('can add name and create client', () => {
        cy.get('[data-cy=create-client-button]').contains('Create Client').click();
        cy.get('app-clients chef-modal').should('exist');
        cy.get('[data-cy=client-name]').type(clientName);
        cy.get('[data-cy=checkbox]').click().should('be.visible');
        cy.get('[data-cy=create]').click();
        cy.get('app-clients chef-modal').should('not.be.visible');
        // cy.get('[data-cy=Download]').click();
        cy.get('[data-cy=close]').click();
        // verify success notification and then dismiss it
        // so it doesn't get in the way of subsequent interactions
        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();
      });
  
      it('fails to create a Client with a duplicate name', () => {
        cy.get('[data-cy=create-client-button]').contains('Create Client').click();
        cy.get('app-clients chef-modal').should('exist');
        cy.get('[data-cy=client-name]').type(clientName);

        cy.get('[data-cy=create]').click();
        cy.get('app-clients chef-modal chef-error').contains('already exists')
        .should('be.visible');
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-clients chef-modal').should('not.be.visible');
      });
  
      it('can cancel creating a Client', () => {
        cy.get('[data-cy=create-client-button]').contains('Create Client').click();
        cy.get('app-clients chef-modal').should('exist');
        cy.get('[data-cy=client-name]').type(clientName);

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-data-bags-list chef-modal').should('not.be.visible');
      });
  
      it('can check create button is disabled until all inputs are filled in', () => {
        cy.get('[data-cy=create-client-button]').contains('Create Client').click();
        cy.get('app-clients chef-modal').should('exist');
        cy.get('[data-cy=client-name]').clear();

        // check for disabled
        cy.get('[data-cy=create]')
        .invoke('attr', 'disabled')
          .then(disabled => {
          disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=create]').click();
          });

        cy.get('app-clients chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-clients  chef-modal').should('not.be.visible');
      });
    });

  function getClient() {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/clients/${clientName}`
    });
  }

  function checkClient(response) {
    if (response.body.clients === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('tr [data-cy=name]').contains('Name');
      cy.get('tr [data-cy=client_name]').contains('Client Name');
      cy.get('tr [data-cy=client_org]').contains('Org Name');
      cy.get('tr [data-cy=json]').contains('JSON');
      return true;
    }
  }

  describe('infra client details page', () => {
    xit('check if Clients tab has a list or not', () => {
      getClient().then((response) => {
        checkClient(response);
      });
    });

    it('can go to Client details', () => {
      cy.get('[data-cy=search-filter]').type(clientName);
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=clients-table-container]').contains(clientName).click();

  });

    it('clicks the Reset key button', () => {
      cy.get('[data-cy=reset-button]').click();
      cy.get('[data-cy=final-reset]').contains('Reset Key').click();
      cy.get('app-reset-client-key chef-modal').should('exist');
    });

    it('downloads the key and closes the modal', () => {
      cy.get('[data-cy=download-key]').click();
      cy.get('[data-cy=close]').click();
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });
  });

  context('can delete Clients', () => {
    it('can delete multiple Clients', () => {
      cy.wait(1000)
      cy.go('back');
      cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-clients-${now}`);
      cy.get('[data-cy=search-entity]').click();

      cy.get('[data-cy=clients-table-container]').contains(clientName).should('exist');
      cy.get('app-clients [data-cy=clients-table-container] chef-td a')
        .contains(clientName).parent().parent().find('.mat-select-trigger').click();
      // we throw in a `should` so cypress retries until introspection allows
      // menu to be shown

      cy.get('[data-cy=delete]').should('be.visible')
        .click();
      // accept dialog
      cy.get('app-clients chef-button').contains('Delete').click();
      // verify success notification and then dismiss it
      cy.get('app-notification.info')
        .contains(`Successfully deleted client - ${clientName}.`);
      cy.get('app-notification.info chef-icon').click();
      cy.get('[data-cy=search-filter]').clear();
      cy.get('[data-cy=search-entity]').click();
      getClients().then((response) => {
        checkResponse(response);
      });
    });
  });
});