describe('infra policy details', () => {
  let adminIdToken = '';
  const serverID = 'chef-server-dev-test';
  const serverName = 'chef server dev';
  const orgID = 'chef-org-dev';
  const orgName = '4thcoffee';
  const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
  const serverIP = '34.219.25.251';
  const adminUser = 'chefadmin';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  let policyFileName = '';
  let revision = '';

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

  function getPolicyFileDetails() {
    const url = `/api/v0/infra/servers/${serverID}/orgs/${orgID}/policyfiles`;
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `${url}/${policyFileName}?revision_id=${revision}`
    });
  }

  function checkIncludedPolicyResponse(response: any) {
    if (response.body.included_policy_locks.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=included-policy-table-container] chef-th').contains('Policy Files');
      cy.get('[data-cy=included-policy-table-container] chef-th').contains('Revision ID');
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

    it('can go to details page', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=policy-file-table-container] chef-td').contains(policyFileName).click();
      }
    });

    it('displays infra node details', () => {
      if (policyFileName !== '') {
        cy.get('.page-title').contains(policyFileName);
        cy.get('[data-cy=policy-file-head]').contains(policyFileName);
        cy.get('[data-cy=policy-file-server]').contains(serverID);
        cy.get('[data-cy=policy-file-org]').contains(orgID);
      }
    });


    it('can check if included policy have data or not', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=included-policy]').contains('Included Policies').click();
        getPolicyFileDetails().then(response => {
          checkIncludedPolicyResponse(response);
        });
      }
    });

    it('can check if run list have data or not', () => {
      if (policyFileName !== '') {
        cy.get('[data-cy=run-list]').contains('Run List').click();
        getPolicyFileDetails().then(response => {
          checkRunlistResponse(response);
        });
      }
    });
  });
});
