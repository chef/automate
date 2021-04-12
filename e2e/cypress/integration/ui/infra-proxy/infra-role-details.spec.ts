describe('infra role detail', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = `-----BEGIN RSA PRIVATE KEY-----\nMIIEpAIBAAKCAQEAln/p9MU/A5J33Q5WdLEC0tlFFo5Tth5fZHSPBJMoKBmQlzbH\naega5xsyiCkHZ0kT+jVOlENayklzyAjEGHS0pi4DYX5GNxGISi6yxAVj6OD1faof\n5ECwdfIkotxDsDTwiRNm7ja49XN/RR9iLqCC66ZdhY8ILvZ6NIXDJ5Vs43COLJsS\nC7oR6x7p4osYpJ/PhHDfGvEjVCH/y28Ozv74nVE2hgY3ym1YO2SzYh1dZztoOMWU\nWKZdgTWfq3Lo1wmQNv4XWb3kTWJHWKlL+XUmgfyLK7r/HAbeqaX9fVBWSvfJibff\npGIQK5wrw3AR37vF0B03NLvyHU6QdFBFFvHkHQIDAQABAoIBAH0buJERp2CA0cOh\nt50pyP8ePqCRkGVEumf3vSxAaJFtLxWFJCCWIkccBNXLxavGxCSrS7dUhpTCms0e\n/GSYH9RFS+ov3o7ItFN2noT1NijRWUItunU0kXx63pnEIUDJwWsyBc7hDsB8UsBT\nZnr8U9kxY20zicoAe3ZN+/1b6jjmgePQTrUHcX0kvaObsfRHwNLYuHiuD8IMftLX\nrWzPZy873S7N8GkGdrUDhh+9D7y0NCOFeSQe6RZNWE+/1wbOXPquZ3LSzyAe8pPQ\ntq2nVUx4f94h8CYfR/XzJI+SUETV+SZQkZ87r2TYQOsBD8DEY3f/pDe9yccNHuzb\nA+SrWsECgYEAx/Bj0/+drvQJ7xqGHLsrf9gbdXDGsMloXq2WvG7FstiPDKP3qQ42\n1BLQ2ZJzm9JpKcfIgRi3rTiKf2bwU9Lrsxz/CZvNCXcxi4KDTlCOG+C03sfAANKz\nvXKCpHeBG8+r/CK8jRjJpAWwA5rCbfaZ5KWW+roPdnHH0CAicMgqXQcCgYEAwLLF\nGhWhWwyhXy/0V6LPtd99IbUcpjFGN/AaVOk5Gngh0F1OPeFSQp6kkH3sckq/N/B0\naDy8oSXAyRF/UT6VicFzp0LRMYJx57sRHZ7WVj8tAB0mKCAztkegVB99bfLiQQ1R\nOE6iWmb+2bomLyRbhoMAM6XlFtN92f6mC1f0kLsCgYBYqvsaoVnEpOVi7FhdlYQN\nBkHnK0RyUl++3SzkFBwI3JFUAcNrbapTEqUcWB59FCsfJEJ/Pf73CwQgy/34rqlo\nnYtdL4MWl42ZWR/yMzdSlaygv+UeeFLNyWK2nWjcdJTJFH6Z9Ew4OW19q7xeF+bX\nx7fVKX6CAKOkYRvk+GARMQKBgQC91B5pSN+woyuhastJPcFjCGvrtdAoRChJWMWH\n2kz/r1KYQiKewQZZTJEPKo2wNcRT5hO20AZ+tYNKUGtc7MtBbopxPlh4bmmpf9Yn\nmN7LDedV0mFRbA+lRMBDvtXAZ2HN9cGKN6SmbAopEMEm9akYRJsBRi79IpE7HCoU\nyKvLmwKBgQDCojS4ANw4C8YrMOsbzWacADdN7vgSLgOyiJf1/pr2ieMUtiCpDUiY\nYwCuViYqyjGRbnkHgcHJSdybxIsn4cBUHQbUFmS4fmOSSmfW5fgmZ3i+iUPqSBP2\n5elo4PFX8XVqjCf2tiPrnJBAKLxp9yY+XWJwfvc8gz/VoyxVOEQ6+Q==\n-----END RSA PRIVATE KEY-----`;
  const roleName = 'chef-load-test-1';
  const roleDescription = 'role description';
  const defaultAttribute = {default: "test"};
  const overrideAttribute = {override: "test"};
  const runlist = ["recipe[aix::nim_master_setup]", "recipe[aix::nim_master_setup_standalone]"];
  const validJson = '{"test":"test"}';
  const invalidJson = '{"invalid "test"';
  const nestedJson = '{"id":"0001", "type": "donut", "batters": {"batter": [{ "id": "1001", "type": "Regular" }, { "id": "1002", "type": "Chocolate" }]} }';
  
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

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: `/api/v0/infra/servers/${serverID}/orgs`,
        body: {
          id: orgID,
          server_id: serverID,
          name: orgName,
          admin_user: adminUser,
          admin_key: adminKey
        }
      });
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'GET',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}`,
        body: {
          id: orgID,
          server_id: serverID
        }
      });
      cy.request({
        auth: { bearer: adminIdToken },
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
      });

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

  describe('infra role details page', () => {
    it('displays role details', () => {
      cy.get('.page-title').contains(roleName);
    });
    
    // details page specs
    it('run list details page', () => {
      cy.get('[data-cy=expand-runlist]').contains('Expand All');
      cy.get('[data-cy=collapse-runlist]').contains('Collapse All');
      cy.get('[data-cy=edit-runlist]').contains('Edit');

      cy.get('#runlist-table-container th').contains('Version');
      cy.get('#runlist-table-container th').contains('Position');
    });

    it('can select environment and load run list data', () => {
      cy.get('.version-dropdown .selected-value .option-content').contains('_default').should('exist');
      
      cy.get('.version-dropdown .selected-value').contains('_default').click();
      cy.get('.version-dropdown .options .option-content').contains('_default').click();
      cy.get('.version-dropdown .selected-value').contains('_default').should('exist');
    });

    it('can search a item from list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=search-roles-and-recipes]').type('aix');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').should('exist');
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').clear();
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').type('test');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('test').should('exist');
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').clear();
      cy.wait(2000);
      cy.get('[data-cy=search-roles-and-recipes]').type('example');
      cy.get('.cdk-virtual-scroll-content-wrapper .no-data').contains('Not Available').should('exist');
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').clear();
      cy.wait(1000);

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');

    });

    it('can scroll to bottom of list to load more data', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-viewport').scrollTo('bottom');

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');

    });

    // drag-drop specs
    it('can select available roles and recipes, available roles, available recipes and load respective data', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available roles and recipes').should('exist');
      
      cy.get('.select-box .mat-select ').contains('available roles and recipes').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text [alt="Available roles"]').click();
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available roles').should('exist');

      cy.get('.select-box .mat-select ').contains('available roles').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text [alt="Available recipes"]').click();
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available recipes').should('exist');

      cy.get('.select-box .mat-select ').contains('available recipes').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text').contains('available roles and recipes').click();
      cy.get('.select-box .mat-select .mat-select-value-text').contains('available roles and recipes').should('exist');

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');

    });

    it('can select a item from list, move to right then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('audit').click();
    
      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('audit').should('exist');
    });

    it('can select multiple item from list, move to right then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('centos-cookbook-file').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('cron').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('learn').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('logrotate').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('poise').click();
      
      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
      
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('cron').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('learn').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('logrotate').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('poise').should('exist');
    });

    it('can select a item from selected run list, move to left then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('aix::nim_master_setup').click();
      
      cy.get('[data-cy=drag-left]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
      
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
    });

    it('can select multiple item from selected run list, move to left then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('learn').click();
      cy.get('.vertical #updated-run-list').contains('poise').click();

      cy.get('[data-cy=drag-left]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
        
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
    });

    it('can select a item from selected run list, move item up then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('aix::nim_master_setup_standalone').click();
        
      cy.get('[data-cy=drag-up]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
        
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('aix::nim_master_setup_standalone').should('exist');
    });
  
    it('can select multiple item from selected run list, move item up then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('centos-cookbook-file').click();
      cy.get('.vertical #updated-run-list').contains('aix::nim_master_setup_standalone').click();
      cy.wait(2000);
      cy.get('[data-cy=drag-up]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
          
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('aix::nim_master_setup_standalone').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file').should('exist');
    });

    it('can expand a runlist', () => {
      cy.get('[data-cy=expand-runlist]').contains('Expand All').click();
      cy.get('#runlist-table-container th').contains('Version');
      cy.get('#runlist-table-container th').contains('Position');
      cy.wait(2000);
    });

    it('can collapse a runlist', () => {
      cy.get('[data-cy=collapse-runlist]').contains('Collapse All').click();
      cy.get('#runlist-table-container th').contains('Version');
      cy.get('#runlist-table-container th').contains('Position');
    });
    
    it('can select a item from selected run list, move item down then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('logrotate').click();
    
      cy.get('[data-cy=drag-down]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
        
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('logrotate').should('exist');
    });
  
    it('can select multiple item from selected run list, move item down then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('aix::nim_master_setup_standalone').click();
      cy.get('.vertical #updated-run-list').contains('centos-cookbook-file').click();
      cy.get('.vertical #updated-run-list').contains('cron').click();

      cy.get('[data-cy=drag-down]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
        
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);

      cy.get('#runlist-table-container td #run-list-name').contains('aix::nim_master_setup_standalone').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('cron').should('exist');
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
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');
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
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');
    });

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

    it('edit default attribute', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', validJson).trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
    });

    it('can edit default attribute and add nested json', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', nestedJson).trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
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
      cy.get('[data-cy=cancel-default-attr-button]').contains('Cancel').should('be.visible').click();
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
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=update-default-attribute]').click();
      });

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attr-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('fails to edit default attribute with a invalid json', () => {
      cy.get('[data-cy=edit-default-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=default-attribute]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-role-details chef-modal chef-error').contains('Must be a valid JSON object')
      .should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-role-details chef-modal chef-button.close').first().click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    });

    it('edit override attribute', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', validJson).trigger('change').type(' ');

      cy.get('[data-cy=update-override-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
    });

    it('can edit override attribute and add nested json', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', nestedJson).trigger('change').type(' ');

      cy.get('[data-cy=update-override-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    
      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
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
      cy.get('[data-cy=cancel-override-attr-button]').contains('Cancel').should('be.visible').click();
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
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=update-override-attribute]').click();
      });

      cy.get('app-infra-role-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attr-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-role-details  chef-modal').should('not.be.visible');
    });

    it('fails to edit default attribute with a invalid json', () => {
      cy.get('[data-cy=edit-override-attribute]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=override-attribute]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-role-details chef-modal chef-error').contains('Must be a valid JSON object')
      .should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-role-details chef-modal chef-button.close').first().click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      cy.wait(2000);
    });

  });
});