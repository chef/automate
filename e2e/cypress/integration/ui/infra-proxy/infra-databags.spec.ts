describe('infra databag', () => {
  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  let adminIdToken = '';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAln/p9MU/A5J33Q5WdLEC0tlFFo5Tth5fZHSPBJMoKBmQlzbH\naega5xsyiCkHZ0kT+jVOlENayklzyAjEGHS0pi4DYX5GNxGISi6yxAVj6OD1faof\n5ECwdfIkotxDsDTwiRNm7ja49XN/RR9iLqCC66ZdhY8ILvZ6NIXDJ5Vs43COLJsS\nC7oR6x7p4osYpJ/PhHDfGvEjVCH/y28Ozv74nVE2hgY3ym1YO2SzYh1dZztoOMWU\nWKZdgTWfq3Lo1wmQNv4XWb3kTWJHWKlL+XUmgfyLK7r/HAbeqaX9fVBWSvfJibff\npGIQK5wrw3AR37vF0B03NLvyHU6QdFBFFvHkHQIDAQABAoIBAH0buJERp2CA0cOh\nt50pyP8ePqCRkGVEumf3vSxAaJFtLxWFJCCWIkccBNXLxavGxCSrS7dUhpTCms0e\n/GSYH9RFS+ov3o7ItFN2noT1NijRWUItunU0kXx63pnEIUDJwWsyBc7hDsB8UsBT\nZnr8U9kxY20zicoAe3ZN+/1b6jjmgePQTrUHcX0kvaObsfRHwNLYuHiuD8IMftLX\nrWzPZy873S7N8GkGdrUDhh+9D7y0NCOFeSQe6RZNWE+/1wbOXPquZ3LSzyAe8pPQ\ntq2nVUx4f94h8CYfR/XzJI+SUETV+SZQkZ87r2TYQOsBD8DEY3f/pDe9yccNHuzb\nA+SrWsECgYEAx/Bj0/+drvQJ7xqGHLsrf9gbdXDGsMloXq2WvG7FstiPDKP3qQ42\n1BLQ2ZJzm9JpKcfIgRi3rTiKf2bwU9Lrsxz/CZvNCXcxi4KDTlCOG+C03sfAANKz\nvXKCpHeBG8+r/CK8jRjJpAWwA5rCbfaZ5KWW+roPdnHH0CAicMgqXQcCgYEAwLLF\nGhWhWwyhXy/0V6LPtd99IbUcpjFGN/AaVOk5Gngh0F1OPeFSQp6kkH3sckq/N/B0\naDy8oSXAyRF/UT6VicFzp0LRMYJx57sRHZ7WVj8tAB0mKCAztkegVB99bfLiQQ1R\nOE6iWmb+2bomLyRbhoMAM6XlFtN92f6mC1f0kLsCgYBYqvsaoVnEpOVi7FhdlYQN\nBkHnK0RyUl++3SzkFBwI3JFUAcNrbapTEqUcWB59FCsfJEJ/Pf73CwQgy/34rqlo\nnYtdL4MWl42ZWR/yMzdSlaygv+UeeFLNyWK2nWjcdJTJFH6Z9Ew4OW19q7xeF+bX\nx7fVKX6CAKOkYRvk+GARMQKBgQC91B5pSN+woyuhastJPcFjCGvrtdAoRChJWMWH\n2kz/r1KYQiKewQZZTJEPKo2wNcRT5hO20AZ+tYNKUGtc7MtBbopxPlh4bmmpf9Yn\nmN7LDedV0mFRbA+lRMBDvtXAZ2HN9cGKN6SmbAopEMEm9akYRJsBRi79IpE7HCoU\nyKvLmwKBgQDCojS4ANw4C8YrMOsbzWacADdN7vgSLgOyiJf1/pr2ieMUtiCpDUiY\nYwCuViYqyjGRbnkHgcHJSdybxIsn4cBUHQbUFmS4fmOSSmfW5fgmZ3i+iUPqSBP2\n5elo4PFX8XVqjCf2tiPrnJBAKLxp9yY+XWJwfvc8gz/VoyxVOEQ6+Q==\n-----END RSA PRIVATE KEY-----"
  const databagName = `${cypressPrefix}-databag-${now}-1`;

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

  function getDatabag() {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/databags`
    });
  }

  function checkResponse(response: any) {
    if (response.body.data_bags === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=create-databag-button]').contains('Create Data Bag');
      cy.get('[data-cy=databags-table-container] chef-th').contains('Name');
      return true;
    }
  }

  describe('infra databags list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // databags tabs specs
    it('can switch to databags tab', () => {
      cy.get('.nav-tab').contains('Data Bags').click();
    });

    it('can check if databag has list or not', () => {
      getDatabag().then((response) => {
        checkResponse(response);
      });
    });

    context('can search and change page in dataBag', () => {
      it('can search a DataBag and check if empty or not', () => {
        cy.get('[data-cy=search-filter]').type(databagName);
        cy.get('[data-cy=search-entity]').click();
        cy.get('[data-cy=databags-table-container]').then(body => {
          if (body.text().includes(databagName)) {
            cy.get('[data-cy=databags-table-container] [data-cy=select-run-list]')
            .contains(databagName).should('exist');
          }
        });
        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getDatabag().then((response) => {
          checkResponse(response);
        });
      });
    });

    // In create databag pop-up details tab specs
    context('can create databag with details tab', () => {
      it('can add name and create databag', () => {
        cy.get('[data-cy=create-databag-button]').contains('Create Data Bag').click();
        cy.get('app-data-bags-list chef-modal').should('exist');
        cy.get('[data-cy=databag-name]').type(databagName);

        cy.get('[data-cy=add-button]').click();
        cy.get('app-data-bags-list chef-modal').should('not.be.visible');

        // verify success notification and then dismiss it
        // so it doesn't get in the way of subsequent interactions
        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();
      });

      it('fails to create a databag with a duplicate name', () => {
        cy.get('[data-cy=create-databag-button]').contains('Create Data Bag').click();
        cy.get('app-data-bags-list chef-modal').should('exist');
        cy.get('[data-cy=databag-name]').type(databagName);

        cy.get('[data-cy=add-button]').click();
        cy.get('app-data-bags-list chef-modal chef-error').contains('already exists')
          .should('be.visible');

        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-data-bags-list chef-modal').should('not.be.visible');
      });

      it('can cancel creating a databag', () => {
        cy.get('[data-cy=create-databag-button]').contains('Create Data Bag').click();
        cy.get('app-data-bags-list chef-modal').should('exist');
        cy.get('[data-cy=databag-name]').type(databagName);

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-data-bags-list chef-modal').should('not.be.visible');
      });

      it('can check create databag button is disabled until all inputs are filled in', () => {
        cy.get('[data-cy=create-databag-button]').contains('Create Data Bag').click();
        cy.get('app-data-bags-list chef-modal').should('exist');
        cy.get('[data-cy=databag-name]').clear();

        // check for disabled
        cy.get('[data-cy=add-button]')
        .invoke('attr', 'disabled')
        .then(disabled => {
          disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-button]').click();
        });

        cy.get('app-data-bags-list chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-data-bags-list  chef-modal').should('not.be.visible');
      });
    });

    // delete databag spec
    context('can delete databags', () => {
      it('can delete multiple databags', () => {
        cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-databag-${now}`);
        cy.get('[data-cy=search-entity]').click();

            cy.get('[data-cy=databags-table-container]').contains(databagName).should('exist');
            cy.get('app-data-bags-list [data-cy=databags-table-container] chef-td a')
              .contains(databagName).parent().parent().find('.mat-select-trigger').click();
            // we throw in a `should` so cypress retries until introspection allows
            // menu to be shown

            cy.get('[data-cy=delete]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-data-bags-list chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info')
            .contains(`Successfully deleted data bag - ${databagName}.`);
            cy.get('app-notification.info chef-icon').click();

        getDatabag().then((response) => {
          checkResponse(response);
        });

        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getDatabag().then((response) => {
          checkResponse(response);
        });
      });
    });
  });
});
