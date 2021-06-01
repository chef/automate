describe('infra node detail', () => {
  let adminIdToken = '';
  let tags: string[];

  const serverID = 'chef-server-dev-test';
  const serverName = 'chef server dev';
  const orgID = 'chef-org-dev';
  const orgName = '4thcoffee';
  const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
  const serverIP = '34.219.25.251';
  const adminUser = 'chefadmin';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const nodeName = 'node-learn_chef_apache2';
  let environment = '';

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
      }).then((response) => {
        if (response.status === 200 && response.body.ok === true) {
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

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/nodes/${nodeName}`
      }).then((response) => {
        if (response.status === 200) {
          tags = response.body.tags;
          environment = response.body.environment;
          cy.request({
            auth: { bearer: adminIdToken },
            failOnStatusCode: false,
            method: 'PUT',
            url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/nodes/${nodeName}/tags`,
            body: {
              org_id: orgID,
              server_id: serverID,
              name: nodeName,
              action: 'delete',
              tags: tags
            }
          }).then((response1) => {
            if (response1.status === 200 && response1.body.ok === true) {
              return;
            }
          });
        }
      });

      cy.request({
        auth: { bearer: adminIdToken },
        failOnStatusCode: false,
        method: 'PUT',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/nodes/${nodeName}/environment`,
        body: {
          org_id: orgID,
          server_id: serverID,
          name: nodeName,
          environment: '_default'
        }
      }).then((response) => {
        if (response.status === 200 && response.body.ok === true) {
          return;
        }
      });

      cy.visit(`/infrastructure/chef-servers/${serverID}/organizations/${orgID}/nodes/${nodeName}`);
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

  function getRunlist(name: string) {
    return cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/node/${nodeName}/runlist/${name}`
    });
  }

  function checkResponse(response: any) {
    if (response.body.run_list.length === 0) {
      cy.get('[data-cy=empty-runlist]').scrollIntoView().should('be.visible');
    } else {
      cy.get('[data-cy=node-expand-runlist]').contains('Expand All');
      cy.get('[data-cy=node-collapse-runlist]').contains('Collapse All');
      cy.get('[data-cy=node-edit-runlist]').contains('Edit');
      cy.get('[data-cy=runlist-table-container] th').contains('Version');
      cy.get('[data-cy=runlist-table-container] th').contains('Position');
      return true;
    }
  }

  describe('inside details tab ', () => {
    // details tab specs

    it('displays infra node details', () => {
      cy.get('.page-title').contains(nodeName);
      cy.get('[data-cy=infra-node-head]').contains(nodeName);
      cy.get('[data-cy=node-server]').contains(serverID);
      cy.get('[data-cy=node-org]').contains(orgID);
    });

    it('can check environments list available or not in dropdown', () => {
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/environments`
      }).then((environmentResponse) => {
        expect(environmentResponse.status).to.equal(200);
        if (environmentResponse.body.environments.length === 0) {
          cy.get('.ng-dropdown-panel-items').should('not.be.visible');
        } else {
          cy.get('.ng-arrow-wrapper').click();
          cy.get('.ng-dropdown-panel-items').should(('be.visible'));
          cy.get('.ng-arrow-wrapper').click();
        }
      });
    });

    it('can select environment and display the confirmation box', () => {
      cy.get('.ng-arrow-wrapper').click();
      cy.get('.ng-dropdown-panel-items').should(('be.visible'));
      cy.get('.ng-option').contains('chef-environment-971654600').click();
      cy.get('[data-cy=change-confirm]').should(('be.visible'));
    });

    it('can cancel the environemnt update', () => {
      cy.get('.ng-arrow-wrapper').click();
      cy.get('.ng-dropdown-panel-items').should(('be.visible'));
      cy.get('.ng-option').contains('chef-environment-885598100').click();
      cy.get('[data-cy=change-confirm]').should(('be.visible'));
      cy.get('[data-cy=cancel-button]').click();
      cy.get('[data-cy=change-confirm]').should(('not.be.visible'));
    });

    it('can update the environemnt', () => {
      cy.get('.ng-arrow-wrapper').click();
      cy.get('.ng-dropdown-panel-items').should(('be.visible'));

      cy.get('.ng-option').contains('chef-environment-275303900').click();
      cy.get('[data-cy=change-confirm]').should(('be.visible'));
      cy.get('[data-cy=save-button]').click();
      cy.get('.ng-value').contains('chef-environment-275303900');

      cy.get('app-notification.info').contains('Successfully updated node environment.');
      cy.get('app-notification.info chef-icon').click();
    });

    it('can add the node tags', () => {
      cy.get('[data-cy=add-tag]').clear().type('tag1');
      cy.get('[data-cy=add-tag-button]').click();

      cy.get('app-notification.info').contains('Successfully updated node tags.');
      cy.get('app-notification.info chef-icon').click();
      cy.get('[data-cy=tag-box]').scrollIntoView();
      cy.get('[data-cy=tag-box]').should(('be.visible'));
      cy.get('.display-node-tags').find('span').should('have.length', 1);
    });

    it('can show the node tags box', () => {
      cy.get('[data-cy=tag-box]').should(('be.visible'));
      cy.get('.display-node-tags').find('span').should('have.length', 1);
    });

    it('can add the multiple node tags', () => {
      cy.get('[data-cy=add-tag]').clear().type('tag2, tag3');
      cy.get('[data-cy=add-tag-button]').click();

      cy.get('app-notification.info').contains('Successfully updated node tags.');
      cy.get('app-notification.info chef-icon').click();
      cy.get('[data-cy=tag-box]').scrollIntoView();
      cy.get('[data-cy=tag-box]').should(('be.visible'));
      cy.get('.display-node-tags').find('span').should('have.length', 3);
    });

    it('can remove the node tags', () => {
      cy.get('[data-cy=remove-tag]').eq(0).click();

      cy.get('app-notification.info').contains('Successfully updated node tags.');
      cy.get('app-notification.info chef-icon').click();
      cy.get('[data-cy=tag-box]').scrollIntoView();
      cy.get('[data-cy=tag-box]').should(('be.visible'));
      cy.get('.display-node-tags').find('span').should('have.length', 2);
    });
  });

  describe('inside run list tab ', () => {

    // switch to Run list tab specs
    it('can switch to run list tab', () => {
      cy.get('[data-cy=run-list-tab]').contains('Run list').click();
      cy.get('.default').contains('Run List');
    });

    it('can check if node has run list or not', () => {
      getRunlist(environment).then((response) => {
        checkResponse(response);
      });
    });

    it('can select multiple item from list, move to right then update the run list', () => {
      cy.get('[data-cy=node-edit-runlist]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');

      cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]')
        .contains('test').click();
      cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('chef')
        .click();


      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(environment).then((runlistResponse) => {
        checkResponse(runlistResponse);
      });
    });

    it('can select a item from selected run list, move to left then update the run list', () => {
      cy.get('[data-cy=node-edit-runlist]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('test').click();
      cy.get('.vertical [data-cy=updated-run-list]').contains('chef').click();

      cy.get('[data-cy=drag-left]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(environment).then((runlistResponse) => {
        checkResponse(runlistResponse);
      });
    });

    it('can check save button is disabled until run list value is not changed', () => {
      cy.get('[data-cy=node-edit-runlist]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=drag-right]').should('be.disabled');
      cy.get('[data-cy=drag-left]').should('be.disabled');
      cy.get('[data-cy=drag-up]').should('be.disabled');
      cy.get('[data-cy=drag-down]').should('be.disabled');

      // check for disabled save button
      cy.get('[data-cy=update-run-list]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=update-run-list]').click();
      });

      cy.get('app-infra-node-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-node-details  chef-modal').should('not.be.visible');
    });

    it('can cancel edit run list', () => {
      cy.get('[data-cy=node-edit-runlist]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=drag-right]').should('be.disabled');
      cy.get('[data-cy=drag-left]').should('be.disabled');
      cy.get('[data-cy=drag-up]').should('be.disabled');
      cy.get('[data-cy=drag-down]').should('be.disabled');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-node-details  chef-modal').should('not.be.visible');
    });

    // switch to Run list tab specs
    it('can switch to details tab', () => {
      cy.get('[data-cy=details-tab]').contains('Details').click();
    });

    it('can update the environemnt and check run list updated or not', () => {
      // can change the environment
      cy.get('.ng-arrow-wrapper').click();
      cy.get('.ng-dropdown-panel-items').should(('be.visible'));

      cy.get('.ng-option').contains('chef-environment-').click();
      cy.get('[data-cy=change-confirm]').should(('be.visible'));
      cy.get('[data-cy=save-button]').click();
      cy.get('.ng-value').contains('chef-environment-');

      cy.get('app-notification.info').contains('Successfully updated node environment.');
      cy.get('app-notification.info chef-icon').click();

      // can switch to Run list tab specs and check run list
      cy.get('[data-cy=run-list-tab]').contains('Run list').click();
      cy.get('.default').contains('Run List');

      getRunlist(environment).then((response) => {
        checkResponse(response);
      });
    });
  });
});
