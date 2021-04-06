describe('chef server details', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const updatedServerName = `${cypressPrefix} updated server ${now}`;
  const serverID = serverName.split(' ').join('-');
  const customServerID = `${cypressPrefix}-custom-id-${now}`;
  const serverFQDN = 'chef-server-1617089723092818000.com';
  const serverIP = '176.119.50.159';
  const orgName = `${cypressPrefix} org ${now}`;
  const generatedOrgID = orgName.split(' ').join('-');
  const customOrgID = `${cypressPrefix}-custom-id-${now}`;
  const adminUser = 'test_admin_user';
  const adminKey = '-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEAxKVaH46oqVJsfV156On1Cojxl/CtDVE5HtyuUVudKqNLVy2x\nnmkuXtLwOCW5P/zwjPis3Qu/IcekPThAJf74Acc8RZR8u5teHk9SvbmZ72vdh0Pw\nV2nTXSRWLA+u5jnvQGLQg2xj5z0dHaI99xwWi6xsBdsgDquLLiWEkbLBRX4bF2r1\nnp4qf71Ka6R61Rmy/UtrTy77Y39j7NBo8WCUzk8240HoU5Wf7nnzfJ1m0AHXiptx\nUWwy7/HM8qcoMiLdSYvI507Aus0ZGZW6endzfM5Q9Xpxgq1WhhP6rzyZFbMAeb7B\nWH36jbuiZEAEvNXZE7QRjztvJgy9VSEaJ5ZOjQIDAQABAoIBAFfaQ6Q7mNBkuYFc\npJ1RAJ+fRMgLx1ReyDUohFxmoJWu9HbTPDo4ZXYJqTZ8bAHRYCqq4peyqsZNqeuL\n2OTgvy4UM7ImP3+VTvwdliqa49HaD9Nhn9t9tOc016bBrvK+SUwrmVpNr2hcnkhh\n9msSymPGOVB4cB79tqV8L3jkMoJ3pXNuTOSpU9VX3mtYH5qjCiGAjh8M5RW+DFcm\n4JBZZ/7o4eSMuj9qzmDQCKLhDvPzOPbdtV067iAaCaF1N9ek/zbSyoUfk6WGCIon\nLRy37BvVcL0mVQjXqTWamHic+53lRf9WDj5fJ5Y2ka2cRm8VzK3bp/wDkEgdSMjR\n9rguvKECgYEA/SwpYRqij+r0gEtnIRgrjXQEOwht/eWL1sCGwlFDcOxDQnm59L9Q\nOcw4PZnCKEv8JV5WrpJIZC3VJyB6RrwA/OT5x/4iw8nRxq2xWIRKZMdb3yDP4pbA\ned+oVOwvLcr7EArenccpmEUutv/v+g83DSzkIrshTq7kz62tqikpf3UCgYEAxteT\nqTYwtROnl7oKj2hSpWVfFDsUyVKpSt6ZnwIGAH7TrvoAsRnw1Xw28X/4cKsNMtmK\nb3c0bQYDIvq6lV5F1vOUFN06Nzc2SZAMgQ3nzU0QDuJthvsh/toqgg1BF0tgDz1g\n2LGScgb0oq36Dtz5Dq/tMt1E5/EWYY59kz8JB7kCgYEA8yvLwv02T0258tB6CguR\nKZjZx2ndXBVzL55U0agEQx5rrBoHRtHLWPiSvHhHSTVSxwHJ+HuBFNgA+Ef1qqNB\nv6afVD0BX1UmLNMAmKjYW9wwniyCAH4T+fudT5Rb7HwekdYe5SU7CorIx/Ukpuae\nVgVcSw+6ejz6gY+sUtieh1ECgYEAwuH2rxmVk2O4FlUYlIVCuygnj8R4EvkZQ2/4\nUIfDKikjf5M3qlwqVpJvpzItZP/A592eeLD0iQYjfN0QkeTbvljtAaXxsLxEUMUF\n/FaMogKtgmkZv/nSz90zXFNxQEt30nftu3QOfAOlMOwi8P5Se7qhWADV0B3SObtW\nCEL3rsECgYEAi//NWyRo14hS8KJubZlm3ocF58IBCVhhuwYleWeqB8Q1xu3PKV1G\neDv5RBsbwyd3u+xvm04cQ/CfUxqgtOdYOfnHfBusFb5GVLOYUpYG4iM6gwuAwFBz\nzcFOpxm6bpGL8soD6JdMmcZ6dRTtlkhhJj/vO4f86V52m3x9LIsiRmY=\n-----END RSA PRIVATE KEY-----';


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
          ip_address: serverIP
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
      cy.get('chef-breadcrumbs').contains('Chef Servers');
      cy.get('chef-breadcrumbs').contains(serverName);
  
      cy.get('.page-title').contains(serverName);
      cy.contains('Add Chef Organization');
    });

    it('can add a organization', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization').click();
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
      cy.get('app-notification.info chef-icon').click();
      cy.get('app-chef-server-details chef-tbody chef-td').contains(orgName).should('exist');
    });

    it('lists organizations', () => {
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization');
      cy.get('#org-table-container chef-th').contains('Name');
      cy.get('#org-table-container chef-th').contains('Admin');
      cy.get('#org-table-container chef-th').contains('Projects');
    });

    it('can create a chef organization with a custom ID', () => {
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
      cy.get('app-chef-server-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

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
      cy.get('@controlMenu').scrollIntoView().should('be.visible')
        .click();
      cy.get('[data-cy=delete-org]').should('be.visible')
        .click();

      // accept dialog
      cy.get('app-chef-server-details chef-button').contains('Delete').click();

      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains('Successfully deleted org');
      cy.get('app-notification.info chef-icon').click();

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
      cy.get('[data-cy=update-server-ip-address]').clear().type(serverIP);

      // check for disabled save button
      cy.get('[data-cy=save-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=save-button]').click();
      });
    });

    it('can check save button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=update-server-name]').clear().type(serverName);
      cy.get('[data-cy=update-server-fqdn]').clear().type(serverFQDN);
      cy.get('[data-cy=update-server-ip-address]').clear();

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
      cy.get('[data-cy=update-server-ip-address]').clear().type(serverIP);
      cy.get('[data-cy=save-button]').click();

      cy.get('app-notification.info').contains('Successfully updated server');
      cy.get('app-notification.info chef-icon').click();
    });
  });
});
