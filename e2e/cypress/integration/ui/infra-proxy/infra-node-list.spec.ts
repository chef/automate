describe('infra node list', () => {
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
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const nodeName = `${cypressPrefix}-node-${now}-1`;
  const seachableNode = 'ec2-node-viv';

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

  function getNodes(node: string, page: number, per_page = 9) {
    const wildCardSearch = '*';
    const target = nodeName !== '' ?
    'name:' + wildCardSearch + node : wildCardSearch + ':';
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

  function checkResponse(response: any) {
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
      return true;
    }
  }

  describe('infra node list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // nodes tabs specs
    it('can switch to nodes tab', () => {
      cy.get('.nav-tab').contains('Nodes').click();
    });

    it('can check if node has list or not', () => {
      getNodes('', 1).then((response) => {
        checkResponse(response);
      });
    });

    context('can search and change the page', () => {
      it('can search a node and check if empty or not', () => {
        cy.get('[data-cy=search-filter]').type(nodeName);
        cy.get('[data-cy=search-entity]').click();
        getNodes(nodeName, 1).then((response) => {
          checkResponse(response);
        });
        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getNodes('', 1).then((response) => {
          checkResponse(response);
        });
      });

      it('can change page and load data according to page', () => {
        const page = 3;
        cy.get('[data-cy=nodes-table-container] chef-th').contains('Node');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('Platform');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('FQDN');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('IP Address');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('Uptime');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('Last Check-In');
        cy.get('[data-cy=nodes-table-container] chef-th').contains('Environment');

        if (cy.get('.nodes-list-paging .page-picker-item').contains('3')) {
          cy.get('.nodes-list-paging .page-picker-item').contains('3').click();
          getNodes('', page).then((response) => {
            checkResponse(response);
          });
        }
      });
    });

    it('can delete node', () => {
      cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-node-${now}`);
      cy.get('[data-cy=search-entity]').click();
      getNodes(`${cypressPrefix}-node-${now}`, 1).then((response) => {
        if (checkResponse(response)) {
          cy.get('[data-cy=nodes-table-container]').contains(nodeName).should('exist');
          cy.get('app-infra-nodes [data-cy=nodes-table-container] chef-td a')
            .contains(nodeName).parent().parent().find('.mat-select-trigger').click();

          cy.get('[data-cy=delete]').should('be.visible')
            .click();
          // accept dialog
          cy.get('app-infra-nodes chef-button').contains('Delete').click();
          // verify success notification and then dismiss it
          cy.get('app-notification.info').contains(`Successfully deleted role - ${nodeName}.`);
          cy.get('app-notification.info chef-icon').click();
        }
      });

      getNodes(`${cypressPrefix}-role-${now}`, 1).then((response) => {
        checkResponse(response);
      });

      cy.get('[data-cy=search-filter]').clear();
      cy.get('[data-cy=search-entity]').click();
      getNodes('', 1).then((response) => {
        checkResponse(response);
      });
    });

    it('can successfully reset the node key', () => {
      cy.get('[data-cy=search-filter]').type(`${seachableNode}`);
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=nodes-table-container]').contains(seachableNode).should('exist');
      cy.get('app-infra-nodes [data-cy=nodes-table-container] chef-td a')
        .contains(seachableNode).parent().parent().find('.mat-select-trigger').click();

      cy.get('[data-cy=reset-key]').should('be.visible')
        .click();
      // accept dialog
      cy.get('app-infra-nodes chef-button').contains('Reset Key').click();
      cy.get('[data-cy=close]').click();
      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains(`Successfully reset the key - ${seachableNode}.`);
      cy.get('app-notification.info chef-icon').click();
    });

    it('can successfully reset the node key and copy the key', () => {
      cy.get('[data-cy=search-filter]').clear();
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=search-filter]').type(`${seachableNode}`);
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=nodes-table-container]').contains(seachableNode).should('exist');
      cy.get('app-infra-nodes [data-cy=nodes-table-container] chef-td a')
        .contains(seachableNode).parent().parent().find('.mat-select-trigger').click();

      cy.get('[data-cy=reset-key]').should('be.visible')
        .click();
      // accept dialog
      cy.get('app-infra-nodes chef-button').contains('Reset Key').click();
      cy.get('[data-cy=copy]').click();
      cy.get('[data-cy=close]').click();
      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains(`Successfully reset the key - ${seachableNode}.`);
      cy.get('app-notification.info chef-icon').click();
    });

    it('can successfully reset the node key and download the the key', () => {
      cy.get('[data-cy=search-filter]').clear();
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=search-filter]').type(`${seachableNode}`);
      cy.get('[data-cy=search-entity]').click();
      cy.get('[data-cy=nodes-table-container]').contains(seachableNode).should('exist');
      cy.get('app-infra-nodes [data-cy=nodes-table-container] chef-td a')
        .contains(seachableNode).parent().parent().find('.mat-select-trigger').click();

      cy.get('[data-cy=reset-key]').should('be.visible')
        .click();
      // accept dialog
      cy.get('app-infra-nodes chef-button').contains('Reset Key').click();
      cy.get('[data-cy=download]').click();
      cy.get('[data-cy=close]').click();
      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains(`Successfully reset the key - ${seachableNode}.`);
      cy.get('app-notification.info chef-icon').click();
    });
  });
});
