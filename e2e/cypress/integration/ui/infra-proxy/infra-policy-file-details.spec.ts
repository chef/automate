describe('infra policy details', () => {
  let adminIdToken = '';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = "-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAln/p9MU/A5J33Q5WdLEC0tlFFo5Tth5fZHSPBJMoKBmQlzbH\naega5xsyiCkHZ0kT+jVOlENayklzyAjEGHS0pi4DYX5GNxGISi6yxAVj6OD1faof\n5ECwdfIkotxDsDTwiRNm7ja49XN/RR9iLqCC66ZdhY8ILvZ6NIXDJ5Vs43COLJsS\nC7oR6x7p4osYpJ/PhHDfGvEjVCH/y28Ozv74nVE2hgY3ym1YO2SzYh1dZztoOMWU\nWKZdgTWfq3Lo1wmQNv4XWb3kTWJHWKlL+XUmgfyLK7r/HAbeqaX9fVBWSvfJibff\npGIQK5wrw3AR37vF0B03NLvyHU6QdFBFFvHkHQIDAQABAoIBAH0buJERp2CA0cOh\nt50pyP8ePqCRkGVEumf3vSxAaJFtLxWFJCCWIkccBNXLxavGxCSrS7dUhpTCms0e\n/GSYH9RFS+ov3o7ItFN2noT1NijRWUItunU0kXx63pnEIUDJwWsyBc7hDsB8UsBT\nZnr8U9kxY20zicoAe3ZN+/1b6jjmgePQTrUHcX0kvaObsfRHwNLYuHiuD8IMftLX\nrWzPZy873S7N8GkGdrUDhh+9D7y0NCOFeSQe6RZNWE+/1wbOXPquZ3LSzyAe8pPQ\ntq2nVUx4f94h8CYfR/XzJI+SUETV+SZQkZ87r2TYQOsBD8DEY3f/pDe9yccNHuzb\nA+SrWsECgYEAx/Bj0/+drvQJ7xqGHLsrf9gbdXDGsMloXq2WvG7FstiPDKP3qQ42\n1BLQ2ZJzm9JpKcfIgRi3rTiKf2bwU9Lrsxz/CZvNCXcxi4KDTlCOG+C03sfAANKz\nvXKCpHeBG8+r/CK8jRjJpAWwA5rCbfaZ5KWW+roPdnHH0CAicMgqXQcCgYEAwLLF\nGhWhWwyhXy/0V6LPtd99IbUcpjFGN/AaVOk5Gngh0F1OPeFSQp6kkH3sckq/N/B0\naDy8oSXAyRF/UT6VicFzp0LRMYJx57sRHZ7WVj8tAB0mKCAztkegVB99bfLiQQ1R\nOE6iWmb+2bomLyRbhoMAM6XlFtN92f6mC1f0kLsCgYBYqvsaoVnEpOVi7FhdlYQN\nBkHnK0RyUl++3SzkFBwI3JFUAcNrbapTEqUcWB59FCsfJEJ/Pf73CwQgy/34rqlo\nnYtdL4MWl42ZWR/yMzdSlaygv+UeeFLNyWK2nWjcdJTJFH6Z9Ew4OW19q7xeF+bX\nx7fVKX6CAKOkYRvk+GARMQKBgQC91B5pSN+woyuhastJPcFjCGvrtdAoRChJWMWH\n2kz/r1KYQiKewQZZTJEPKo2wNcRT5hO20AZ+tYNKUGtc7MtBbopxPlh4bmmpf9Yn\nmN7LDedV0mFRbA+lRMBDvtXAZ2HN9cGKN6SmbAopEMEm9akYRJsBRi79IpE7HCoU\nyKvLmwKBgQDCojS4ANw4C8YrMOsbzWacADdN7vgSLgOyiJf1/pr2ieMUtiCpDUiY\nYwCuViYqyjGRbnkHgcHJSdybxIsn4cBUHQbUFmS4fmOSSmfW5fgmZ3i+iUPqSBP2\n5elo4PFX8XVqjCf2tiPrnJBAKLxp9yY+XWJwfvc8gz/VoyxVOEQ6+Q==\n-----END RSA PRIVATE KEY-----"
  let policyFileName = '';
  let revision = '';
  let includedPolicyFileName = '';
  let includedPolicyRevision = '';
  const defaultAttribute = {default: 'test'};
  const overrideAttribute = {override: 'test'};

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
      cy.get('[data-cy=policy-file-table-container] chef-th').contains('Name');
      cy.get('[data-cy=policy-file-table-container] chef-th').contains('Revision ID');
      policyFileName = response.body.policies[0].name;
      revision = response.body.policies[0].revision_id;
      return true;
    }
  }

  function getPolicyFileDetails(policyName: string, policyRevision: string) {
    const url = `/api/v0/infra/servers/${serverID}/orgs/${orgID}/policyfiles`;
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `${url}/${policyName}?revision_id=${policyRevision}`
    });
  }

  function checkIncludedPolicyResponse(response: any) {
    if (response.body.included_policy_locks.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=included-policy-table-container] chef-th').contains('Policyfiles');
      cy.get('[data-cy=included-policy-table-container] chef-th').contains('Revision ID');
      includedPolicyFileName = response.body.included_policy_locks[0].name;
      includedPolicyRevision = response.body.included_policy_locks[0].revision_id;
      return true;
    }
  }

  function checkIncludedPolicyDetailsResponse(response: any) {
    if (response.status !== 200) {
      cy.get('[data-cy=empty-list]').should('be.visible');
      cy.get('[data-cy=close-policy-button]').click();
    } else {
      cy.get('[data-cy=policyfile-heading]').contains(includedPolicyFileName);
      cy.get('[data-cy=policyfile-info]').contains('POLICYFILE INFORMATION');
      cy.get('[data-cy=policyfile-meta-info]').contains('METADATA');
      return true;
    }
  }

  function checkRunlistResponse(response: any) {
    if (response.body.cookbook_locks.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=run-list-table-container] chef-th').contains('Run Items');
      cy.get('[data-cy=run-list-table-container] chef-th').contains('Current Version');
      cy.get('[data-cy=run-list-table-container] chef-th').contains('Source');
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

  describe('infra policyfile list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // policyfile tabs specs
    it('can switch to policyfile tab', () => {
      cy.get('.nav-tab').contains('Policyfiles').click();
    });

    it('can check if policyfile has list or not', () => {
        getPolicyFile().then((response) => {
          checkResponse(response);
        });
    });

    it('can go to details page', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=policy-file-table-container] chef-td').contains(policyFileName).click();
      }
    });

    it('displays policyfile details', () => {
      if (policyFileName !== '') {
        cy.get('.page-title').contains(policyFileName);
        cy.get('[data-cy=policy-file-head]').contains(policyFileName);
        cy.get('[data-cy=policy-file-server]').contains(serverID);
        cy.get('[data-cy=policy-file-org]').contains(orgID);
      }
    });

    describe('cookbook dependencies page', () => {
      it('can click cookboook button', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=cookbook-dependencies-button]')
          .contains('Cookbook Dependencies').click();
          cy.get('[data-cy=cookbook-dependencies-heading]')
          .contains('Cookbook Dependencies');
        }
      });

      it('can open & close dependency rule accordion', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=dependency-rule]').contains('Dependencies Rules').click();
          cy.get('[data-cy=dependency-rule-arrow]').click();
        }
      });

      it('can open cookbook accordion', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=cookbook]').contains('Cookbook').click();
        }
      });

      it('can open cookbook details page', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=cookbook-table-container] chef-td a').contains(policyFileName).click();
          cy.get('[data-cy=close-cookbook-detail-button]').click();
          cy.get('[data-cy=close-cookbook-button]').click();
        }
      });
    });

    describe('included policyfiles details page', () => {
      it('can check if included policy have data or not', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=included-policy]').contains('Included Policies').click();
          getPolicyFileDetails(policyFileName, revision).then(response => {
            checkIncludedPolicyResponse(response);
          });
        }
      });

      it('can go to includedd policy file details page', () => {
        if (includedPolicyFileName !== '') {
          cy.get('[data-cy=included-policy-table-container] chef-td')
          .contains(includedPolicyFileName).click();
          getPolicyFileDetails(includedPolicyFileName, includedPolicyRevision).then(response => {
            if (checkIncludedPolicyDetailsResponse(response)) {
              cy.get('[data-cy=close-policy-button]').click();
            }
          });
        }
      });

      it('can click go to details page button', () => {
        if (includedPolicyFileName !== '') {
          cy.get('[data-cy=included-policy-table-container] chef-td')
          .contains(includedPolicyFileName).click();
          getPolicyFileDetails(includedPolicyFileName, includedPolicyRevision).then(response => {
            if (checkIncludedPolicyDetailsResponse(response)) {
              cy.get('[data-cy=policyfile-details]').contains('Go to Policyfile Details >').click();
            }
          });
        }
      });
    });

    describe('run list accordion', () => {
      it('can check if run list have data or not', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=run-list]').contains('Run List').click();
          getPolicyFileDetails(policyFileName, revision).then(response => {
            checkRunlistResponse(response);
          });
        }
      });

      it('can open runlist details slider', () => {
        if (policyFileName !== '') {
          cy.get('[data-cy=run-list-table-container] chef-td a').contains(policyFileName).click();
          cy.get('[data-cy=close-cookbook-detail-button] chef-icon').click();
        }
      });
    });

    it('can show revision id of policyfile', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=revisions]').contains('Revisions').click();
        getRevisionId(policyFileName).then((revisionResponse) => {
          checkRevisionIdResponse(revisionResponse);
          cy.get('[data-cy=close-revision-id-button]').click();
        });
      }
    });
  });

  describe('inside attribute tab ', () => {

    // attribute tab specs
    it('can can switch to attribute tab', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=attributes-tab]').contains('Attributes').click();
      }
    });

    it('attribute page', () => {
      if (policyFileName !== '') {
        cy.get('.default').contains('Default Attributes');
        cy.get('[data-cy=expand-default-attribute]').contains('Expand All');
        cy.get('[data-cy=collapse-default-attribute]').contains('Collapse All');

        cy.get('.override').contains('Override Attributes');
        cy.get('[data-cy=expand-override-attribute]').contains('Expand All');
        cy.get('[data-cy=collapse-override-attribute]').contains('Collapse All');
      }
    });

    it('can expand a default attribute', () => {
      if (policyFileName !== '') {
        cy.get('body').then($body => {
          if ($body.find('.empty-default-attribute').length <= 0) {
            cy.get('[data-cy=expand-default-attribute]').contains('Expand All').click();
            cy.wait(2000);
            cy.get('[data-cy=collapse-default-attribute]').contains('Collapse All').click();
          }
        });
      }
    });

    it('can expand a override attribute', () => {
      if (policyFileName !== '') {
        cy.get('body').then($body => {
          if ($body.find('.empty-override-attribute').length <= 0) {
            cy.get('[data-cy=expand-override-attribute]').contains('Expand All').click();
            cy.wait(2000);
            cy.get('[data-cy=collapse-override-attribute]').contains('Collapse All').click();
          }
        });
      }
    });
  });
});
