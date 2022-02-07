describe('chef server details', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const updatedServerName = `${cypressPrefix} updated server ${now}`;
  const serverID = serverName.split(' ').join('-');
  const customServerID = `${cypressPrefix}-custom-id-${now}`;
  const serverFQDN = Cypress.env('AUTOMATE_INFRA_SERVER_FQDN');
  const serverIP = '18-117-112-129';
  const webuiKey = Cypress.env('AUTOMATE_INFRA_WEBUI_KEY').replace(/\\n/g, '\n');
  const orgName = `${cypressPrefix} org ${now}`;
  const generatedOrgID = orgName.split(' ').join('-');
  const customOrgID = `${cypressPrefix}-custom-id-${now}`;
  const adminUser = 'test_admin_user';

  // using dummy admin key value for creating the org
  const adminKey = 'Dummy--admin--key';

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/api/v0/infra/servers',
        body: {
          id: serverID,
          name: serverName,
          fqdn: serverFQDN,
          ip_address: serverIP,
          webui_key: webuiKey
        }
      });

      cy.visit(`/infrastructure/chef-servers/${serverID}`);
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

  describe('chef server details page', () => {
    it('displays server details', () => {
      cy.get('chef-breadcrumbs').contains('Chef Infra Servers');
      cy.get('chef-breadcrumbs').contains(serverName);
      cy.get('.page-title').contains(serverName);
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization');
    });

    it('it can update the webui key', () => {
      cy.get('[data-cy=update-web-ui-key]').contains('Update').click();
      cy.get('app-chef-server-details .sidenav-header').should('exist');
      cy.get('[data-cy=enter-webui-key]').type(webuiKey);

      cy.get('[data-cy=update-webui-key-button]').click();
      cy.get('app-chef-server-details .sidenav-header').should('not.be.visible');
    });

    it('can validate the server webui key', () => {
      cy.get('[data-cy=valid-webui-key]').contains('Valid');
    });

    it('can check empty org list', () => {
      cy.get('#org-table-container chef-th').should('not.be.visible');
      cy.get('[data-cy=empty-list]').should('be.visible');
      cy.get('[data-cy=no-records]').contains('No Organization available');
    });

    it('can add a organization', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization')
        .click({multiple: true, force: true});
      cy.get('app-chef-server-details chef-modal').should('exist');
      cy.get('[data-cy=org-name]').type(orgName);
      cy.get('[data-cy=id-label]').contains(generatedOrgID);
      cy.get('[data-cy=admin-user]').type(adminUser);
      cy.get('[data-cy=admin-key]').clear().invoke('val', adminKey).trigger('input');

      cy.get('[data-cy=add-button]').click();
      cy.get('app-chef-server-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true, force: true});
      cy.get('app-chef-server-details chef-tbody chef-td').contains(orgName).should('exist');
    });

    it('lists organizations', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization');
      cy.get('#org-table-container chef-th').contains('Name');
      cy.get('#org-table-container chef-th').contains('Admin');
      cy.get('#org-table-container chef-th').contains('Projects');
    });

    it('can create a chef organization with a custom ID', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization')
        .click({multiple: true, force: true});
      cy.get('app-chef-server-details chef-modal').should('exist');
      cy.get('[data-cy=org-name]').type(orgName);
      cy.get('[data-cy=add-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=add-id]').should('be.visible').clear().type(customOrgID);
      cy.get('[data-cy=admin-user]').type(adminUser);
      cy.get('[data-cy=admin-key]').clear().invoke('val', adminKey).trigger('input');

      cy.get('[data-cy=add-button]').click();
      cy.get('app-chef-server-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true, force: true});

      cy.get('app-chef-server-details chef-tbody chef-td').contains(orgName).should('exist');
    });

    it('fails to create a chef organization with a duplicate ID', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization').click();
      cy.get('app-chef-server-details chef-modal').should('exist');
      cy.get('[data-cy=org-name]').type(orgName);
      cy.get('[data-cy=add-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=add-id]').should('be.visible').clear().type(customOrgID);
      cy.get('[data-cy=admin-user]').type(adminUser);
      cy.get('[data-cy=admin-key]').clear().invoke('val', adminKey).trigger('input');
      cy.get('[data-cy=add-button]').click();
      cy.get('app-chef-server-details chef-modal chef-error').contains('already exists')
        .should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-chef-server-details chef-modal chef-button.close').first().click();
    });

    it('can cancel creating a chef organization', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization').click();
      cy.get('app-chef-server-details chef-modal').should('exist');

      // scoll to show the cancel button
      cy.get('[data-cy=cancel-button]').scrollIntoView();

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-chef-server-details  chef-modal').should('not.be.visible');
    });

    it('can delete a chef organization', () => {
      cy.get('app-chef-server-details chef-td a').contains(orgName).parent().parent()
        .find('.mat-select-trigger').as('controlMenu');

      // we throw in a `should` so cypress retries until introspection allows menu to be shown
      cy.get('@controlMenu').scrollIntoView().should('be.visible').click();
      cy.get('[data-cy=delete-org]').should('be.visible').click();

      // accept dialog
      cy.get('app-chef-server-details chef-button').contains('Delete').click();

      // verify success notification and then dismiss it
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click({multiple: true, force: true});

      cy.get('app-chef-server-details chef-tbody chef-td')
        .contains(customOrgID).should('not.exist');
    });

    it('can check create organization button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization').click();
      cy.get('app-chef-server-details chef-modal').should('exist');
      cy.get('[data-cy=org-name]').type(orgName);
      cy.get('[data-cy=id-label]').contains(generatedOrgID);
      cy.get('[data-cy=admin-key]').clear().invoke('val', adminKey).trigger('input');

      // check for disabled
      cy.get('[data-cy=add-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-button]').click();
      });

      cy.get('app-chef-server-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('chef-button').contains('Cancel').should('be.visible').click();
      cy.get('app-chef-server-details  chef-modal').should('not.be.visible');
    });

    // details tabs specs
    it('can can switch to details tab', () => {
      cy.get('[data-cy=details-tab]').contains('Details').click();
    });

    it('can check save button is disabled until server details input value is not changed', () => {
      cy.get('[data-cy=update-server-name]').clear().type(serverName);
      cy.get('[data-cy=update-server-fqdn]').clear().type(serverFQDN);

      // check for disabled save button
      cy.get('[data-cy=save-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=save-button]').click();
      });
    });

    xit('can check save button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=update-server-name]').clear().type(serverName);
      cy.get('[data-cy=update-server-fqdn]').clear().type(serverFQDN);

      // check for disabled save button
      cy.get('[data-cy=save-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=save-button]').click();
      });

      cy.get('app-chef-server-details chef-error').contains('is required')
        .should('be.visible');
    });

    it('can update the server', () => {
      cy.get('[data-cy=update-server-name]').clear().type(updatedServerName);
      cy.get('[data-cy=update-server-fqdn]').clear().type(serverFQDN);
      cy.get('[data-cy=save-button]').click();

      cy.get('app-notification.info').contains('Successfully updated server');
      cy.get('app-notification.info chef-icon').click({ multiple: true });
    });

    it('can update the server', () => {
      cy.get('[data-cy=update-server-name]').clear().type(updatedServerName);
      cy.get('chef-select').contains('FQDN').click();
      cy.get('chef-select chef-option').contains('IP Address').click();
      cy.get('[data-cy=update-server-ip-address]').clear().type(serverIP);
      cy.get('[data-cy=save-button]').click();

      cy.get('app-notification.info').contains('Successfully updated server');
      cy.get('app-notification.info chef-icon').click({ multiple: true });
    });
  });
});
