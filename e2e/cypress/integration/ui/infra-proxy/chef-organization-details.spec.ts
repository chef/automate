describe('chef server', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAln/p9MU/A5J33Q5WdLEC0tlFFo5Tth5fZHSPBJMoKBmQlzbH\naega5xsyiCkHZ0kT+jVOlENayklzyAjEGHS0pi4DYX5GNxGISi6yxAVj6OD1faof\n5ECwdfIkotxDsDTwiRNm7ja49XN/RR9iLqCC66ZdhY8ILvZ6NIXDJ5Vs43COLJsS\nC7oR6x7p4osYpJ/PhHDfGvEjVCH/y28Ozv74nVE2hgY3ym1YO2SzYh1dZztoOMWU\nWKZdgTWfq3Lo1wmQNv4XWb3kTWJHWKlL+XUmgfyLK7r/HAbeqaX9fVBWSvfJibff\npGIQK5wrw3AR37vF0B03NLvyHU6QdFBFFvHkHQIDAQABAoIBAH0buJERp2CA0cOh\nt50pyP8ePqCRkGVEumf3vSxAaJFtLxWFJCCWIkccBNXLxavGxCSrS7dUhpTCms0e\n/GSYH9RFS+ov3o7ItFN2noT1NijRWUItunU0kXx63pnEIUDJwWsyBc7hDsB8UsBT\nZnr8U9kxY20zicoAe3ZN+/1b6jjmgePQTrUHcX0kvaObsfRHwNLYuHiuD8IMftLX\nrWzPZy873S7N8GkGdrUDhh+9D7y0NCOFeSQe6RZNWE+/1wbOXPquZ3LSzyAe8pPQ\ntq2nVUx4f94h8CYfR/XzJI+SUETV+SZQkZ87r2TYQOsBD8DEY3f/pDe9yccNHuzb\nA+SrWsECgYEAx/Bj0/+drvQJ7xqGHLsrf9gbdXDGsMloXq2WvG7FstiPDKP3qQ42\n1BLQ2ZJzm9JpKcfIgRi3rTiKf2bwU9Lrsxz/CZvNCXcxi4KDTlCOG+C03sfAANKz\nvXKCpHeBG8+r/CK8jRjJpAWwA5rCbfaZ5KWW+roPdnHH0CAicMgqXQcCgYEAwLLF\nGhWhWwyhXy/0V6LPtd99IbUcpjFGN/AaVOk5Gngh0F1OPeFSQp6kkH3sckq/N/B0\naDy8oSXAyRF/UT6VicFzp0LRMYJx57sRHZ7WVj8tAB0mKCAztkegVB99bfLiQQ1R\nOE6iWmb+2bomLyRbhoMAM6XlFtN92f6mC1f0kLsCgYBYqvsaoVnEpOVi7FhdlYQN\nBkHnK0RyUl++3SzkFBwI3JFUAcNrbapTEqUcWB59FCsfJEJ/Pf73CwQgy/34rqlo\nnYtdL4MWl42ZWR/yMzdSlaygv+UeeFLNyWK2nWjcdJTJFH6Z9Ew4OW19q7xeF+bX\nx7fVKX6CAKOkYRvk+GARMQKBgQC91B5pSN+woyuhastJPcFjCGvrtdAoRChJWMWH\n2kz/r1KYQiKewQZZTJEPKo2wNcRT5hO20AZ+tYNKUGtc7MtBbopxPlh4bmmpf9Yn\nmN7LDedV0mFRbA+lRMBDvtXAZ2HN9cGKN6SmbAopEMEm9akYRJsBRi79IpE7HCoU\nyKvLmwKBgQDCojS4ANw4C8YrMOsbzWacADdN7vgSLgOyiJf1/pr2ieMUtiCpDUiY\nYwCuViYqyjGRbnkHgcHJSdybxIsn4cBUHQbUFmS4fmOSSmfW5fgmZ3i+iUPqSBP2\n5elo4PFX8XVqjCf2tiPrnJBAKLxp9yY+XWJwfvc8gz/VoyxVOEQ6+Q==\n-----END RSA PRIVATE KEY-----"
    
  const tabNames = ['Roles', 'Environments', 'Data Bags', 'Clients', 'Cookbooks'];

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

    it('on page load Cookbook is in active state', () => {
      cy.get('.nav-tab').contains('Cookbooks').should('have.class', 'active');
    });

    tabNames.forEach((val) => {
      it(`can switch to ${val} tab`, () => {
          cy.get('.nav-tab').contains(val).click();
      });
    });

    it('lists of Cookbook', () => {
      cy.get('.cookbooks').then(($cookbook) => {
        if ($cookbook.hasClass('empty-section')) {
          cy.get('[data-cy=cookbooks-table-container]').should('not.be.visible');
            cy.get('.empty-section').should('be.visible');
            cy.get('.empty-section p').contains('No cookbooks available');
        } else {
          cy.get('[data-cy=cookbooks-table-container] chef-th').contains('Name');
          cy.get('[data-cy=cookbooks-table-container] chef-th').contains('Cookbook Version');
        }
      });
    });

  });
});
