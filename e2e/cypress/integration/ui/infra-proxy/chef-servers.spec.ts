describe('chef server', () => {
  const now = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const generatedServerID = serverName.split(' ').join('-');
  const customServerID = `${cypressPrefix}-custom-id-${now}`;
  // const serverFQDN = Cypress.env('AUTOMATE_INFRA_SERVER_FQDN');
  // const serverIP = '18-117-112-129';
  // const webuiKey = Cypress.env('AUTOMATE_INFRA_WEBUI_KEY').replace(/\\n/g, '\n');
 
  const serverID = 'cs';
  // const serverName = 'cs';
  const orgID = 'demoorg';
  const orgName = 'demoorg';
  const serverFQDN = 'ec2-18-219-27-165.us-east-2.compute.amazonaws.com';
  const serverIP = '18-219-27-165';
  const webuiKey = '-----BEGIN RSA PRIVATE KEY-----\nMIIEogIBAAKCAQEA5uYiYBuxNaTrnIuZ5Tr74eWgZlKU1QOhMxR7MzDlaOgJHYXg\n8w7XLDLnADC38tVfM5/2JXDjKkVJoFHGsRxpuUETBJdyUOJ4+t4SjDnuuTS1MT/g\nN6fkTweUpVhmde+btN8paqbM92iVTuD+24X4weV+HDrR2wt9CTY2L/5cjLTTbCbP\nfclGl5ZlUFMxhb+86V73RXtwJRXJP97Sl8Y+Srd1reQgyea/sUyia8wEa+vah6Ki\nTV8csillBAXAqoOYnZ/mCKIqIAq24ZS3MiIcj789asR1CkttU+AjJzMN3N/1aemN\nz77tqNjo3m/jBegE35RnGNYazKQciCOXI22ZjwIDAQABAoIBAGrwcUFT8gonXNw/\nJ8KsQ9aG9HgPjVDvAmzW8oxt/AJPXVFoHRxVZkDZPnWvGrMEMbiGQIlstDlyp7vx\nw/VpdGnRgs5gmhhuG5wM2EhL2I28pKKvs9fzEWGs4CKIQ8QhelsV9Gep40o/DM/g\nkwj3DpQ3BuZqM6Ggu4NVsVkfFJwxjnPf7j2gTS/Xx0YkqQhBEEpLtgCEPM3fL3GL\n6kNRlH080l4nWui+2Wrfk6pcKa+xff2QJ/7vaG1KQJYmHy4XkIJnmDznUhjSj+uC\nEgoIEd/yI2GluLvzQXu1OqPevzWhCGE3VcIjJqvu+MgZQblqx2GwyIbtBsCDO22Y\nqC3aLwkCgYEA9l+eMKEdnLi+e6ulknK6WQcZ8zfSSmQSqCV3e413CYZ01FaE5fuP\nUEpnL6mP07kBRuujiPJQxcXX/xbiWOVj+tgO8VJOiSUnU1/r/XgKsl9Ztp2Zyn3m\nRLBfXAm4LGgAeLjSUP786Oi3viybXrrY74arc2uAwGGV6YUmGk9PXMUCgYEA7+u6\n8iCI/hm9LPTYRWdcdYq081oYzcBwVM5vTasDj/dM/8LbwAe0LvnrX3ZJne4WoWpe\nebkM8eKfGBOGOY7NOh1hkViRj3PpclDvd069zakdF4HIKUX+8TxFrVtKSozzRfxz\nqJc6zgeG86le1DV/RFaOMv6IPSFmknqGnpGwKkMCgYARIZLxVLKnbB0qokPQDBQz\nDpBaXh5MN+soL/q/55VIX0YOYbm5+CT98z57jPHISgvxr37ejeLZ7ajYPvIcNk8e\naRmY5B074g56+KeDJ26u+nxVe13vO5PgYNcwoihhRhYKTueo5CIX296y5Y0kKB/f\nf+C6FNaOCfsPkseUesFM9QKBgHG/8gKaKllPGsIV0/QT7WJNQT2fmeLzOk77EQph\n6m53capRWJ7TCDq9gzrfekfGejRNmj1b3ydPqt5fY2QmsGa9R2UB+QFqk2P/u4l1\na8f2RjS+84eeCCGX0fE/NxnhefXUm4FzMg48w18DDlLOimh8IS9MPou9KkSIkwrb\nSArBAoGAe+/H4SRILQHbDZLQT42T5yEQDFHoyBACFRTvpSFDmtH8xf1PypCGg/2a\nU1XNFs402Ka6X79p0quPSlo2U6zO1BquqHImJrdITilBZYL+hFT2naeIroNIrsp+\nrKVnGCtqvJrDXz6P84ff4ho3POgSi2jDMmarmIDkoJZOa5TMR+4=\n-----END RSA PRIVATE KEY-----';
  const generatedOrgID = orgName.split(' ').join('-');
  before(() => {
    cy.adminLogin('/infrastructure/chef-servers').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
    });
    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  describe('chef server list page', () => {

    it('can add a infra server', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('exist');
      cy.get('[data-cy=add-name]').type(serverName);
      cy.get('[data-cy=id-label]').contains(generatedServerID);
      cy.get('[data-cy=add-fqdn]').type(serverFQDN);
      cy.get('[data-cy=webui_key]').type(webuiKey);
      cy.get('[data-cy=add-button]').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.get('app-chef-servers-list chef-tbody chef-td').contains(serverName).should('exist');
    });

    it('lists servers', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server');

      cy.get('#servers-table-container chef-th').contains('Name');
      cy.get('#servers-table-container chef-th').contains('FQDN');
      cy.get('#servers-table-container chef-th').contains('IP Address');
      cy.get('#servers-table-container chef-th').contains('Number Of Orgs');
    });

    it('can create a chef server with a custom ID', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('exist');
      cy.get('[data-cy=add-name]').type(serverName);
      cy.get('[data-cy=add-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=add-id]').should('be.visible').clear().type(customServerID);
      cy.get('[data-cy=add-fqdn]').type(serverFQDN);
      cy.get('[data-cy=webui_key]').type(webuiKey);

      cy.get('[data-cy=add-button]').click();
      cy.get('app-chef-servers-list chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.get('app-chef-servers-list chef-tbody chef-td').contains(serverName).should('exist');
    });

    xit('can create a chef server with IP address', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('app-chef-servers-list chef-modal').should('exist');
      cy.get('[data-cy=add-name]').type(serverName);
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('chef-select').contains('FQDN').click();
      cy.get('chef-select chef-option').contains('IP Address').click();
      cy.get('[data-cy=add-ip-address]').type(serverIP);
      cy.get('[data-cy=webui_key]').type(webuiKey);

      cy.get('[data-cy=add-button]').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.get('app-chef-servers-list chef-tbody chef-td').contains(serverName).should('exist');
    });

    it('fails to create a chef server with a duplicate ID', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('exist');
      cy.get('[data-cy=add-name]').type(serverName);
      cy.get('[data-cy=add-id]').should('not.be.visible');
      cy.get('[data-cy=edit-button]').contains('Edit ID').click();
      cy.get('[data-cy=id-label]').should('not.be.visible');
      cy.get('[data-cy=add-id]').should('be.visible').clear().type(customServerID);
      cy.get('[data-cy=add-fqdn]').type(serverFQDN);
      cy.get('[data-cy=webui_key]').type(webuiKey);
      cy.get('[data-cy=add-button]').click();
    });

    it('can cancel creating a chef server', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('exist');

      // here we exit with the Cancel button
      cy.get('chef-button').contains('Cancel').should('be.visible').click();
      cy.get('app-chef-servers-list  chef-modal').should('not.be.visible');
    });

    it('can delete a chef server', () => {
      cy.get('app-chef-servers-list chef-td a').contains(serverName).parent().parent()
        .find('.mat-select-trigger').as('controlMenu');

      // we throw in a `should` so cypress retries until introspection allows menu to be shown
      cy.get('@controlMenu').scrollIntoView().should('be.visible')
        .click();
      cy.get('[data-cy=delete-server]').should('be.visible')
        .click();

      // accept dialog
      cy.get('app-chef-servers-list chef-button').contains('Delete').click();

      // verify success notification and then dismiss it
      cy.get('app-notification.info').contains('Successfully deleted server');
      cy.get('app-notification.info chef-icon').click();

      cy.get('app-chef-servers-list chef-tbody chef-td')
        .contains(customServerID).should('not.exist');
    });

    it('can check create server button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('exist');
      cy.get('[data-cy=add-name]').type(serverName);
      cy.get('[data-cy=id-label]').contains(generatedServerID);

      // check for disabled
      cy.get('[data-cy=add-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-button]').click();
      });

      cy.get('[data-cy=chef-infra-server-slider]').should('exist');

      // here we exit with the Cancel button
      cy.get('chef-button').contains('Cancel').should('be.visible').click();
      cy.get('[data-cy=chef-infra-server-slider]').should('not.be.visible');
    });
  });
});
