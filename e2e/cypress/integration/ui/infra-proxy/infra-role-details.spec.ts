describe('infra role detail', () => {
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
  const roleName = `${cypressPrefix}-role-${now}`;
  const roleDescription = 'role description';
  const defaultAttribute = {default: 'test'};
  const overrideAttribute = {override: 'test'};
  const runlist = ['recipe[aix::nim_master_setup]', 'recipe[aix::nim_master_setup_standalone]'];
  const validJson = '{"test":"test"}';
  const invalidJson = '{"invalid "test"';
  const nestedJson = '{"id":"0001","batters":{"batter":[{"id":"1001"},{"id": "1002"}]} }';
  const nullJson = '{}';
  const envId = '_default';
  const searchValue = ['aix', 'test', 'exp'];

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
        failOnStatusCode: false,
        method: 'POST',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles`,
        body: {
          org_id: orgID,
          server_id: serverID,
          name: roleName,
          description: roleDescription,
          default_attributes: defaultAttribute,
          override_attributes: overrideAttribute,
          run_list: runlist
        }
      }).then((response) => {
        if (response.status === 200 && response.body.ok === true) {
          return;
        } else {
            cy.request({
              auth: { bearer: adminIdToken },
              method: 'GET',
              url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}`,
              body: {
                id: orgID,
                server_id: serverID,
                name: roleName
              }
            });
          }
      });

      cy.visit(`/infrastructure/chef-servers/${serverID}/organizations/${orgID}/roles/${roleName}`);
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
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}/runlist/${name}`
    });
  }

  function updateRole(attribute: string, jsonValue: any) {
    let roleUpdated: object;
    if (attribute === 'default') {
      roleUpdated = {
        org_id: orgID,
        server_id: serverID,
        name: roleName,
        description: roleDescription,
        default_attributes: JSON.parse(jsonValue.replace(/\r?\n|\r/g, '')),
        override_attributes: overrideAttribute,
        run_list: runlist
      };
    } else {
      roleUpdated = {
        org_id: orgID,
        server_id: serverID,
        name: roleName,
        description: roleDescription,
        default_attributes: defaultAttribute,
        override_attributes: JSON.parse(jsonValue.replace(/\r?\n|\r/g, '')),
        run_list: runlist
      };
    }
    return cy.request({
      auth: { bearer: adminIdToken },
      method: 'PUT',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}`,
      body: roleUpdated
    });
  }

  describe('can visit the details page', () => {
    it('displays role details', () => {
      cy.get('.page-title').contains(roleName);
    });
  });

  describe('inside details tab ', () => {
    // details page specs
    it('can check environments and run list available or not', () => {
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles/${roleName}/environments`
      }).then((environmentResponse) => {
        expect(environmentResponse.status).to.equal(200);
        if (environmentResponse.body.environments.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          getRunlist(environmentResponse.body.environments[0]).then((runlistResponse) => {
            cy.get('[data-cy=expand-runlist]').contains('Expand All');
            cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
            cy.get('[data-cy=edit-runlist]').contains('Edit');
            if (runlistResponse.body.run_list.length === 0) {
              cy.get('[data-cy=empty-runlist]').should('be.visible');
            } else {
              cy.get('[data-cy=runlist-table-container] th').contains('Version');
              cy.get('[data-cy=runlist-table-container] th').contains('Position');
            }
          });
        }
      });
    });

    it('can select environment and load run list data', () => {
      cy.get('.version-dropdown .selected-value .option-content').contains(envId)
        .should('exist');

      cy.get('.version-dropdown .selected-value').contains(envId).click();
      cy.get('.version-dropdown .options .option-content').contains(envId).click();
      cy.get('.version-dropdown .selected-value').contains(envId).should('exist');
      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
        }
      });
    });

    it('can search a item from list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      searchValue.forEach((val) => {
        cy.get('[data-cy=search-roles-and-recipes]').type(val);
        cy.get('.cdk-virtual-scroll-content-wrapper').then(body => {
          if (body.text().includes(val)) {
            cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains(val)
              .should('exist');
          } else {
            cy.get('.cdk-virtual-scroll-content-wrapper .no-data').contains('Not Available')
              .should('exist');
          }
        });
        cy.get('[data-cy=search-roles-and-recipes]').clear();
      });
      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('can scroll to bottom of list to load more data', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-viewport').scrollTo('bottom');

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    // drag-drop specs
    it('can select available roles and recipes, only roles, only recipes to load respective data',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.select-box .mat-select .mat-select-value-text')
        .contains('available roles and recipes').should('exist');

      cy.get('.select-box .mat-select ').contains('available roles and recipes').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text [alt="Available roles"]')
        .click();
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available roles')
        .should('exist');

      cy.get('.select-box .mat-select ').contains('available roles').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text [alt="Available recipes"]')
        .click();
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available recipes')
        .should('exist');

      cy.get('.select-box .mat-select ').contains('available recipes').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text')
        .contains('available roles and recipes').click();
      cy.get('.select-box .mat-select .mat-select-value-text')
        .contains('available roles and recipes').should('exist');

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('can select a item from list, move to right then update the run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]')
        .contains('audit').click();

      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
        }
      });
    });

    it('can select multiple item from list, move to right then update the run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]')
        .contains('audit::inspec').click();
      cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]')
        .contains('chef-client').click();

      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
        }
      });
    });

    it('can select a item from selected run list, move to left then update the run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('aix::nim_master_setup').click();

      cy.get('[data-cy=drag-left]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
        }
      });
    });

    it('can select multiple item from selected run list, move to left then update the run list',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('audit').click();
      cy.get('.vertical [data-cy=updated-run-list]').contains('chef-client').click();

      cy.get('[data-cy=drag-left]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
        }
      });
    });

    it('can select a item from selected run list, move item up then update the run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('aix::nim_master_setup_standalone')
        .click();

      cy.get('[data-cy=drag-up]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('aix::nim_master_setup_standalone').should('exist');
        }
      });
    });

    xit('can select multiple item from selected run list, move item up then update the run list',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('audit').click();
      cy.get('.vertical [data-cy=updated-run-list]').contains('aix::nim_master_setup_standalone')
        .click();
      cy.wait(2000);
      cy.get('[data-cy=drag-up]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('aix::nim_master_setup_standalone').should('exist');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('centos-cookbook-file').should('exist');
        }
      });
    });

    it('can expand a run list', () => {
      cy.get('[data-cy=expand-runlist]').contains('Expand All').click();
      cy.get('[data-cy=runlist-table-container] th').contains('Version');
      cy.get('[data-cy=runlist-table-container] th').contains('Position');
      cy.wait(2000);
    });

    it('can collapse a runlist', () => {
      cy.get('[data-cy=collapse-runlist]').contains('Collapse All').click();
      cy.get('[data-cy=runlist-table-container] th').contains('Version');
      cy.get('[data-cy=runlist-table-container] th').contains('Position');
    });

    xit('can select a item from selected run list, move item down then update the run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('centos-cookbook-file').click();

      cy.get('[data-cy=drag-down]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('centos-cookbook-file').should('exist');
        }
      });
    });

    xit('can select multiple item from selected run list, move item down then update the run list',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical [data-cy=updated-run-list]').contains('aix::nim_master_setup_standalone')
        .click();
      cy.get('.vertical [data-cy=updated-run-list]').contains('centos-cookbook-file').click();
      cy.get('.vertical [data-cy=updated-run-list]').contains('cron').click();

      cy.get('[data-cy=drag-down]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      getRunlist(envId).then((runlistResponse) => {
        cy.get('[data-cy=expand-runlist]').contains('Expand All');
        cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=edit-runlist]').contains('Edit');
        if (runlistResponse.body.run_list.length === 0) {
          cy.get('[data-cy=empty-runlist]').should('be.visible');
        } else {
          cy.get('[data-cy=runlist-table-container] th').contains('Version');
          cy.get('[data-cy=runlist-table-container] th').contains('Position');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('aix::nim_master_setup_standalone').should('exist');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('centos-cookbook-file').should('exist');
          cy.get('[data-cy=runlist-table-container] td [data-cy=run-list-name]')
            .contains('cron').should('exist');
        }
      });
    });

    it('can check save button is disabled until run list value is not changed', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
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

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('can cancel edit run list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=drag-right]').should('be.disabled');
      cy.get('[data-cy=drag-left]').should('be.disabled');
      cy.get('[data-cy=drag-up]').should('be.disabled');
      cy.get('[data-cy=drag-down]').should('be.disabled');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });
  });

  describe('inside attribute tab ', () => {

    // attribute tab specs
    it('can can switch to attribute tab', () => {
      cy.get('[data-cy=attributes-tab]').contains('Attributes').click();
    });

    it('attribute page', () => {
      cy.get('.default').contains('Default Attributes');
      cy.get('[data-cy=expand-default-attribute]').contains('Expand All');
      cy.get('[data-cy=collapse-default-attribute]').contains('Collapse All');
      cy.get('[data-cy=edit-default-attribute]').contains('Edit');

      cy.get('.override').contains('Override Attributes');
      cy.get('[data-cy=expand-override-attribute]').contains('Expand All');
      cy.get('[data-cy=collapse-override-attribute]').contains('Collapse All');
      cy.get('[data-cy=edit-override-attribute]').contains('Edit');
    });

    it('edit default attribute and show empty data', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click({force: true});
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', nullJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      updateRole('default', nullJson).then((response) => {
        if (Object.keys(response.body.default_attributes).length > 0) {
          cy.get('[data-cy=empty-default-attribute]').should('be.visible');
        } else {
          cy.get('[data-cy=empty-default-attribute]').should('not.be.visible');
        }
      });
    });

    it('edit default attribute and show updated data', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click({force: true});
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      updateRole('default', validJson).then((response) => {
        if (Object.keys(response.body.default_attributes).length > 0) {
          cy.get('[data-cy=empty-default-attribute]').should('not.be.visible');
        } else {
          cy.get('[data-cy=empty-default-attribute]').should('be.visible');
        }
      });
    });

    it('can edit default attribute and add nested json', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', nestedJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true});

      updateRole('default', nestedJson).then((response) => {
        if (Object.keys(response.body.default_attributes).length > 0) {
          cy.get('[data-cy=empty-default-attribute]').should('not.be.visible');
        } else {
          cy.get('[data-cy=empty-default-attribute]').should('be.visible');
        }
      });
    });

    it('can expand a default attribute', () => {
      cy.get('[data-cy=expand-default-attribute]').contains('Expand All').click();
      cy.wait(2000);
      cy.get('[data-cy=collapse-default-attribute]').contains('Collapse All').click();
    });

    it('can cancel edit default attribute  role', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear();

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attr-button]').contains('Cancel').should('be.visible')
        .click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('can check default attribute button is disabled until textarea is filled', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').focus();

      // check for disabled
      cy.get('[data-cy=update-default-attribute]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled
          ? cy.log('buttonIsDiabled')
          : cy.get('[data-cy=update-default-attribute]').click();
      });

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attr-button]').contains('Cancel').should('be.visible')
        .click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('fails to edit default attribute with a invalid json', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-role-details chef-modal chef-error')
        .contains('Must be a valid JSON object').should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-role-details chef-modal chef-button.close').first().click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    });

    it('edit override attribute and show empty data', () => {
      cy.get('[data-cy=edit-override-attribute]').scrollIntoView().should('be.visible');
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', nullJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-override-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true});

      updateRole('override', nullJson).then((response) => {
        if (Object.keys(response.body.override_attributes).length > 0) {
          cy.get('[data-cy=empty-override-attribute]').scrollIntoView().should('be.visible');
        } else {
          cy.get('[data-cy=empty-override-attribute]').scrollIntoView().should('not.be.visible');
        }
      });
    });

    it('edit override attribute and show updated data', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');

      cy.get('[data-cy=override-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-override-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true});

      updateRole('override', validJson).then((response) => {
        if (Object.keys(response.body.override_attributes).length > 0) {
          cy.get('[data-cy=empty-override-attribute]').should('not.be.visible');
        } else {
          cy.get('[data-cy=empty-override-attribute]').should('be.visible');
        }
      });
    });

    it('can edit override attribute and add nested json', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', nestedJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-override-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      updateRole('override', nestedJson).then((response) => {
        if (Object.keys(response.body.override_attributes).length > 0) {
          cy.get('[data-cy=empty-override-attribute]').should('not.be.visible');
        } else {
          cy.get('[data-cy=empty-override-attribute]').should('be.visible');
        }
      });
    });

    it('can expand a override attribute', () => {
      cy.get('[data-cy=expand-override-attribute]').contains('Expand All').click();
      cy.wait(2000);
      cy.get('[data-cy=collapse-override-attribute]').contains('Collapse All').click();
    });

    it('can cancel edit override attribute  role', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear();

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attr-button]').contains('Cancel').should('be.visible')
        .click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('can check override attribute button is disabled until textarea is filled', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').focus();

      // check for disabled
      cy.get('[data-cy=update-override-attribute]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') :
          cy.get('[data-cy=update-override-attribute]').click();
      });

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attr-button]').contains('Cancel')
        .should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('fails to edit default attribute with a invalid json', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-role-details chef-modal chef-error')
        .contains('Must be a valid JSON object').should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-role-details chef-modal chef-button.close').first().click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      cy.wait(2000);
    });

    // switch tab specs
    it('can can switch to details tab', () => {
      cy.get('[data-cy=runList-tab]').contains('Details').click();
      cy.get('.default').contains('Run List');
    });

    // delete role spec
    it('can click on breadcrumb and delete role', () => {
      cy.get('.breadcrumbs .breadcrumb').contains('Roles').click();
      cy.get('[data-cy=search-filter]').type(roleName);
      cy.get('[data-cy=search-entity]').click();

      cy.wait(2000);
      cy.get('[data-cy=roles-table-container]').contains(roleName).should('exist');
      cy.get('app-infra-roles [data-cy=roles-table-container] chef-td a').contains(roleName)
        .parent().parent().find('.mat-select-trigger').as('controlMenu');
      // we throw in a `should` so cypress retries until introspection allows menu to be shown
      cy.get('@controlMenu').scrollIntoView().should('be.visible')
        .click();
      cy.get('[data-cy=delete-role]').should('be.visible')
        .click();
      // accept dialog
      cy.get('app-infra-roles chef-button').contains('Delete').click();
      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains(`Successfully deleted role - ${roleName}.`);
      cy.get('app-notification.info chef-icon').click();

      cy.get('[data-cy=search-filter]').clear();
      cy.get('[data-cy=search-entity]').click();
    });
  });
});
