describe('infra databag', () => {
  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  let adminIdToken = '';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'demoorg';
  const orgName = 'demoorg';
  const serverFQDN = Cypress.env('AUTOMATE_INFRA_SERVER_FQDN');
  const adminUser = 'kallol';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const webuiKey = Cypress.env('AUTOMATE_INFRA_WEBUI_KEY').replace(/\\n/g, '\n');

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
          ip_address: '',
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

  function getOrgUsers() {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/automateinfraorgusers`
    });
  }

  function checkResponse(response: any) {
    if (response.body.users === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=orgUsers-table-container] chef-th').contains('Name');
      return true;
    }
  }

  describe('infra organization users list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // organization users tabs specs
    it('can switch to organization users tab', () => {
      cy.get('.nav-tab').contains('Users').click();
    });

    it('can check if organization users has list or not', () => {
      getOrgUsers().then((response) => {
        checkResponse(response);
      });
    });

  });
});
