describe('infra policy group details', () => {
  let adminIdToken = '';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  let policies: any;
  let policyGroupName = '';
  let policyFilesCount: number;
  let policyFileName: string;
  let policyFileRevision: number;
  let nodeName: string;

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
      policyGroupName = policies[0].policy_group;
      cy.get('[data-cy=policy-group-table-container] chef-th').contains('Policy Group');
      cy.get('[data-cy=policy-group-table-container] chef-th').contains('Number of Policyfiles');
      return true;
    }
  }

  function getPolicyGroupDetails(policyName: string) {
    const url = `/api/v0/infra/servers/${serverID}/orgs/${orgID}/policygroups`;
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `${url}/${policyName}`
    });
  }

  function checkPolicyGroupDetailsResponse(response: any) {
    if (response.body.name === '') {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=policy-group-details-table-container] chef-th').contains('Policyfiles');
      cy.get('[data-cy=policy-group-details-table-container] chef-th').contains('Revision ID');
      policies = response.body.policies;
      policyFilesCount = response.body.policies.length;
      policyFileName =  response.body.policies[0].name;
      policyFileRevision =  response.body.policies[0].revision_id;
      return true;
    }
  }

  function getPolicyGroupNodes(policyGroup: string, page: number, per_page = 9) {
    const wildCardSearch = '*';
    const target = policyGroup !== '' ?
    'policy_group:' + wildCardSearch + policyGroup : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = page - 1;
    // Add asterisk to do wildcard search
    const params =
  `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${per_page}`;
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/nodes?${params}`
    });
  }

  function checkNodesResponse(response: any) {
    if (response.body.nodes.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=nodes-table-container] chef-th').contains('Node');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('Platform');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('FQDN');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('IP Address');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('Uptime');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('Last Check-In');
      cy.get('[data-cy=nodes-table-container] chef-th').contains('Environment');
      nodeName = response.body.nodes[0].name;
      return true;
    }
  }

  describe('infra policy groups list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // policy groups tabs specs
    it('can switch to policy groups tab', () => {
      cy.get('.nav-tab').contains('Policy Groups').click();
    });

    it('can check if policy groups has list or not', () => {
        getPolicyFile().then((response) => {
          checkResponse(response);
        });
    });

    it('can go to details page', () => {
      if (policyGroupName !== '') {
        cy.get('[data-cy=policy-group-table-container] chef-td').contains(policyGroupName).click();
      }
    });

    it('displays policyfile details', () => {
      if (policyGroupName !== '') {
        getPolicyGroupDetails(policyGroupName).then((response) => {
          checkPolicyGroupDetailsResponse(response);
        });
        cy.get('.page-title').contains(policyGroupName);
        cy.get('[data-cy=policy-group-server]').contains(serverID);
        cy.get('[data-cy=policy-group-org]').contains(orgID);
      }
    });

    it('can go to policyfile detail page by click on policyfile name', () => {
      if (policyGroupName !== '') {
        getPolicyGroupDetails(policyGroupName).then((response) => {
          checkPolicyGroupDetailsResponse(response);
        });
        cy.get('[data-cy=policy-group-details-table-container] chef-td')
          .contains(policyFileName).click();
          cy.on('url:changed', (newUrl) => { expect(newUrl).to
            .contain('policyfiles/' + policyFileName + '/revision/' + policyFileRevision);
          });
        cy.get('[data-cy=policy-file-head]').contains(policyFileName);
      }
    });

    it('can switch to nodes tab', () => {
      cy.go('back');
      if (policyGroupName !== '') {
        cy.get('[data-cy=nodes-tab]').contains('Nodes').click();
      }
    });

    it('displays policy group nodes', () => {
      if (policyGroupName !== '') {
        getPolicyGroupNodes(policyGroupName, 1, 9).then((response) => {
          checkNodesResponse(response);
        });
      }
    });

    it('can go to node detail page by click on node name', () => {
      if (policyGroupName !== '') {
        getPolicyGroupNodes(policyGroupName, 1, 9).then((response) => {
          checkNodesResponse(response);
        });
        if (nodeName !== '' && nodeName !== undefined) {
          cy.get('[data-cy=nodes-table-container] chef-td').contains(nodeName)
            .click();
            cy.on('url:changed', (newUrl) => {
              expect(newUrl).to
              .contain('nodes/' + nodeName);
            });
          cy.get('[data-cy=infra-node-head]').contains(nodeName);
        } else {
          cy.get('[data-cy=no-nodes]').should('be.visible');
        }
      }
    });
  });
});
