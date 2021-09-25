describe('infra policy file', () => {
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
  let policies: any;
  const policyFileName = `${cypressPrefix}-policyFile-${now}-1`;

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

  function getPolicyFile() {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/policyfiles`
    });
  }

  function checkResponse(response: any) {
    if (response.body.policies.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      policies = response.body.policies;
      cy.get('[data-cy=policy-file-table-container] chef-th').contains('Name');
      cy.get('[data-cy=policy-file-table-container] chef-th').contains('Revision ID');
      return true;
    }
  }

  function getRevisionId(policyFile: string) {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/policyfiles/${policyFile}/revisions`
    });
  }

  function checkRevisionIdResponse(response: any) {
    if (response.body.revisions.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=revision-id-table-container] chef-th').contains('Revision ID');
      return true;
    }
  }

  describe('infra policy file list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // policy file tabs specs
    it('can switch to policy file tab', () => {
      cy.get('.nav-tab').contains('Policyfiles').click();
    });

    it('can check if policy file has list or not', () => {
      getPolicyFile().then((response) => {
        checkResponse(response);
      });
    });

    context('can change page in policy file', () => {

      it('can change page and load data according to page', () => {
        getPolicyFile().then((response) => {
          if (checkResponse(response)) {
            if (policies.length > 9 &&
              cy.get('.policy-file-list-paging .page-picker-item').contains('3')) {
              cy.get('.policy-file-paging .page-picker-item').contains('3').click();
            }
          }
        });
      });
    });

    context('can search and change page in policyfile', () => {
      it('can search a Policyfile and check if empty or not', () => {
        getPolicyFile().then((response) => {
          if (checkResponse(response)) {
            cy.get('[data-cy=search-filter]').type(policyFileName);
            cy.get('[data-cy=search-entity]').click();
            cy.get('[data-cy=policy-file-table-container]').then(body => {
              if (body.text().includes(policyFileName)) {
                cy.get('[data-cy=policy-file-table-container]')
                .contains(policyFileName).should('exist');
              }
            });

            cy.get('[data-cy=search-filter]').clear();
            cy.get('[data-cy=search-entity]').click();
          }
        });
      });
    });

    it('can show revision id of policy file', () => {
      getPolicyFile().then((response) => {
        if (checkResponse(response)) {
          cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-policyFile-${now}`);
          cy.get('[data-cy=search-entity]').click();
          if (policies.some((policy: { name: string; }) =>
            policy.name === `${cypressPrefix}-policyFile-${now}`)) {
            cy.get('app-policy-files [data-cy=policy-file-table-container] chef-td a')
            .contains(`${cypressPrefix}-policyFile-${now}`).parent().parent()
            .find('.mat-select-trigger').click();
            cy.get('[data-cy=revision-id]').should('be.visible')
            .click();
            getRevisionId(`${cypressPrefix}-policyFile-${now}`).then((revisionResponse) => {
              checkRevisionIdResponse(revisionResponse);
              cy.get('[data-cy=close-button]').click();
            });
          } else {
            cy.get('[data-cy=empty-list]').should('be.visible');
          }
          cy.get('[data-cy=search-filter]').clear();
          cy.get('[data-cy=search-entity]').click();
        }
      });
    });

    it('can delete policy file', () => {
      getPolicyFile().then((response) => {
        if (checkResponse(response)) {
          cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-policyFile-${now}`);
          cy.get('[data-cy=search-entity]').click();
          if (policies.includes(`${cypressPrefix}-policyFile-${now}`)) {
            cy.get('[data-cy=policy-file-table-container]').contains(policyFileName)
              .should('exist');
            cy.get('app-policy-files [data-cy=policy-file-table-container] chef-td a')
              .contains(policyFileName).parent().parent().find('.mat-select-trigger').click();

            cy.get('[data-cy=delete]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-policy-files chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info').
              contains(`Successfully deleted policy file - ${policyFileName}.`);
            cy.get('app-notification.info chef-icon').click();

          } else {
            cy.get('[data-cy=empty-list]').should('be.visible');
          }

          cy.get('[data-cy=search-filter]').clear();
          cy.get('[data-cy=search-entity]').click();
        }
      });
    });
  });
});
