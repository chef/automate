describe('infra policy groups', () => {
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
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const policyGroupName = `${cypressPrefix}-policyGroup-${now}-1`;
  let policies: any;

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

  function getPolicyGroups() {
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
      cy.get('[data-cy=policy-group-table-container] chef-th').contains('Policy Group');
      cy.get('[data-cy=policy-group-table-container] chef-th').contains('Number of Policyfiles');
      return true;
    }
  }

  // tslint:disable-next-line:no-shadowed-variable
  function filterPolicyGroup(policies: any) {
    const policyGroups: any = [];
    const key = 'policy_group';
    policies.forEach((x: any) => {
      if (policyGroups.some((val: any) => val[key] === x[key])) {
        policyGroups.forEach((k: any) => {
          if (k[key] === x[key]) {
            k['occurrence']++;
          }
        });
      } else {
        const a: any = [];
        a[key] = x[key];
        a['occurrence'] = 1;
        policyGroups.push(a);
      }
    });
    return policyGroups;
  }

  describe('infra policy group list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // policy group tabs specs
    it('can switch to policy group tab', () => {
      cy.get('.nav-tab').contains('Policy Groups').click();
    });

    it('can check if policy group has list or not', () => {
      getPolicyGroups().then((response) => {
        checkResponse(response);
      });
    });

    context('can search and change page in policyfile', () => {
      it('can search a Policyfile and check if empty or not', () => {
        getPolicyGroups().then((response) => {
          if (checkResponse(response)) {
            cy.get('[data-cy=search-filter]').type(policyGroupName);
            cy.get('[data-cy=search-entity]').click();
            cy.get('[data-cy=policy-group-table-container]').then(body => {
              if (body.text().includes(policyGroupName)) {
                cy.get('[data-cy=policy-group-table-container]')
                .contains(policyGroupName).should('exist');
              }
            });

            cy.get('[data-cy=search-filter]').clear();
            cy.get('[data-cy=search-entity]').click();
          }
        });
      });
    });

    context('can change page in policy file', () => {
      it('can change page and load data according to page', () => {
        getPolicyGroups().then((response) => {
          if (checkResponse(response)) {
            const groupCount = filterPolicyGroup(policies);
            if (groupCount > 100 &&
              cy.get('.policy-group-list-paging .page-picker-item').contains('3')) {
              cy.get('.policy-file-paging .page-picker-item').contains('3').click();
            }
          }
        });
      });
    });
  });
});
