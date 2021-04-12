describe('infra role', () => {
  let adminIdToken = '';
  const serverID = 'chef-server-dev-test';
  const serverName = 'chef server dev';
  const orgID = 'chef-org-dev';
  const orgName = '4thcoffee';
  const serverFQDN = 'ec2-34-219-25-251.us-west-2.compute.amazonaws.com';
  const serverIP = '34.219.25.251';
  const adminUser = 'chefadmin';
  const adminKey = `-----BEGIN RSA PRIVATE KEY-----\nMIIEpQIBAAKCAQEA6KIxELz/HWjwT8qiQrhPbvymVG/hnF/n8owZfh04hsABneB8\nu6xklW+VjCRMBHfptovHn+5NyN5blR1wpXQc5iKXEPfDny/gncNAeu4lrezs8f8z\ndZa2jFOwFH/Cm4o1+po3uhXtAsdtN/WI7EiCHZEeXRA5jFYF86Y92G0FDoaRiA4G\njWu7M3hL/61ELZ80d2RH+GdKgFqQh9hRlnx59ozmMW4maaMXpx2eb8RLvYgKBAUH\n5qGpWdMcODsLBkIy/+RqpLdsMFxrvFWBeTVBQvecx/UZMMWU3yQigzRismiYuLug\nnbN7DhgK71GXPkHG4JoxltgW5E+lYtjZD1wh9wIDAQABAoIBAQCJSGWyHgZjQbFH\nNSqKOyBNO/WgMKIwWPyVSw4kOXRJOPf7RiX1zqdQ9JeJK0ZdALLAUj7M56Gpn2bm\nWYhHa30+Zj1F+yDLSULBdx8PLIi52e5+ZP7mLrmtmBl6D2c1yNtP90BZpWTH1g5j\nDpft8GAwuJn1i4Sah41dmsY2eSeZyLn1RMj8MXW/bXr4QbRtvxjXcW6Sfstj8jrV\nzjG/kbptPzoFTwqUcIR8xmmlewSqOERMhdJXYNW07uVMzmZTKB/UrJQr+lzx4otJ\ng0uHQftcku00/KW6iyGTKbQh9gm4UJ4JfVRg9rrCL2NPvLw6Y3BzlKTVOxj8UFI5\nyoZ/YBLpAoGBAPjwJQptYtgauW6MR7wboa4YK4ltB9Qa7SIbfVGPQISDhfdpyg/6\nLZW65MwHPbWM4NjwwpkaLuBrBY0eqGvxtdWAvLPCBTRfoSay5v9E7qFgWWTNWhuq\nC/SuzJeh1bnSNoNW0/fKDDAPsU+x3gUNKiu0VWFCq5tTk78NWat3wpLDAoGBAO87\no6csdMSrLOaqoUCqUOJNpv6h8uMxLyOPJWgqAIZCVNpZaPOuWseHavkofkI5Hpzu\nT1rBEZZKVjZN8JFXc+1O4n9uqd8kXooONqsRYDDPcKxHqvibXWl42PeNDx/tcETo\ndXiwhjtHuoBjKTyp18+ND8i8wjIb0pEwjLnPBpi9AoGBAKRnZO8YAtmZ/LEHsDCJ\nRyQFDmu6OBtalRWbdg4xP3jEq+sUUMBwhz6FqakddyG5op1jlAEtC5xm0cm7X5u6\njE4usXE+R9Wo+nLmYCxiR/8H9MHf5Qy0JyU3dCMIXHBWZm4jH+Zb5+St2Ho7i9dm\nNF2cwW50a4UvkznwBTJ662OlAoGAU4fsbXutTdpC9EuRwEV3tnzMmU4sw70xq2XY\n9tTrl/VjMVZUufkgA1k6NcM4yqCtGrhvBs4w+Nv93Do8jFMPzEVl+n7GW36Ub/nk\nhrircH+N5OmlPebpp+ElSNJ8/HXoZHcSRVDFnb8+1INLK75V90dWwo199QcX79AW\n4u3xbLUCgYEAm+1Dv8bvC9d3Z08mCJjUbzdRG6qA39EXpixVYjbmXmDpy71KA2zR\nLvgdoNIAiVKFUcR1z8aty8HNJKzzZPL35VpFJ5Sm4Zh99OVDJkRxpWdZvqdL865h\n8/A/e8ZFjAWF8m83OlP0sb1dn8CQ8Pf+hFfW/a97Y7maECqU0oyNXJg=\n-----END RSA PRIVATE KEY-----`;
  const roleName = 'chef-load-test-1';
  const roleDescription = 'role description';
  const roleRunlistName = 'chef-load-test-2';
  const roleDefaultAttrName = 'chef-load-test-3';
  const roleOverrideAttrName = 'chef-load-test-4';
  const validJson = '{"test":"test"}';
  const invalidJson = '{"invalid "test"';

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

  describe('infra role list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // roles tabs specs
    it('can switch to roles tab', () => {
      cy.get('.nav-tab').contains('Roles').click();
    });

    it('lists roles', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role');
      cy.get('#roles-table-container chef-th').contains('Name');
      cy.get('#roles-table-container chef-th').contains('Description');
      cy.get('#roles-table-container chef-th').contains('Environments');
    });

    it('can search a role and check empty', () => {
      cy.get('#search-filter').type(roleName);
      cy.get('[data-cy=search-role]').click();
      cy.get('#roles-table-container chef-th').should('not.be.visible');
      cy.get('[data-cy=empty-list]').should('be.visible');
      cy.get('[data-cy=no-records]').contains('No results');
            
      cy.get('#search-filter').clear();
      cy.get('[data-cy=search-role]').click();
    });

    it('can change page and load data according to page', () => {
      cy.get('#roles-table-container chef-th').contains('Name');
      cy.get('#roles-table-container chef-th').contains('Description');
      cy.get('#roles-table-container chef-th').contains('Environments');
      cy.get('.roles-list-paging').contains('3').click();
      cy.get('.roles-list-paging').contains('chevron_right').click();
      cy.get('.roles-list-paging').contains('last_page').click();
      cy.get('.roles-list-paging').contains('chevron_left').click();
      cy.get('.roles-list-paging').contains('first_page').click({force: true});
    });

    // In create role pop-up details tab specs
    it('can add name, description and create role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('[data-cy=add-button]').click();
      cy.get('app-infra-roles chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });

    it('fails to create a role with a duplicate name', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('[data-cy=add-button]').click();
      cy.get('app-infra-roles chef-modal chef-error').contains('already exists')
        .should('be.visible');

      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can cancel creating a role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleName);

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can check create role button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').clear();
      cy.get('[data-cy=role-description]').clear();

      // check for disabled
      cy.get('[data-cy=add-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-button]').click();
      });

      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    // In create role pop-up run list tab specs
    it('can add a name, description, run list in role', () => {
      cy.wait(1000);
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleRunlistName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('audit').click();
 
      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=add-run-list-button]').click();
      cy.get('app-infra-roles chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });

    it('can cancel creating a run list role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleRunlistName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();

      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-run-list-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can check create run list button is disabled until all inputs are filled in', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleRunlistName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();

      // check for disabled
      cy.get('[data-cy=add-run-list-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-run-list-button]').click();
      });

      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-run-list-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    // In create role pop-up default attribute tab specs
    it('can add a name, description, run list and default attribute in role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('audit').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=add-default-attribute-button]').click();
      cy.get('app-infra-roles chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });

    it('can cancel creating default attribute  role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').focus();
      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can check create default attribute button is disabled until textarea is filled', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear();

      // check for disabled
      cy.get('[data-cy=add-default-attribute-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-default-attribute-button]').click();
      });

      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('failed to create default attribute with invalid json', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-roles chef-modal chef-error').contains('Must be a valid JSON object')
        .should('be.visible');

      cy.wait(1000);
      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    // In create role pop-up override attribute tab specs
    it('can add a name, description, run list, default attribute and override attribute in role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('audit').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('.navbar').contains('Override Attributes').click();
      cy.get('[data-cy=role-override-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=add-override-attribute-button]').click();
      cy.get('app-infra-roles chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });

    it('can cancel creating override attribute  role', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('.navbar').contains('Override Attributes').click();
      cy.get('[data-cy=role-override-attribute]').focus();

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can check create override attribute button is disabled until textarea is filled', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('.navbar').contains('Override Attributes').click();
      cy.get('[data-cy=role-override-attribute]').clear();

      // check for disabled
      cy.get('[data-cy=add-override-attribute-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-override-attribute-button]').click();
      });

      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('failed to create override attribute with invalid json', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);
      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('.navbar').contains('Override Attributes').click();
      cy.get('[data-cy=role-override-attribute]').invoke('val', invalidJson)
        .trigger('change').type(' ');

      cy.get('app-infra-roles chef-modal chef-error').contains('Must be a valid JSON object')
        .should('be.visible');

      cy.wait(1000);
      cy.get('app-infra-roles chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    it('can move to details tab if fails to create a role with a duplicate name', () => {
      cy.get('[data-cy=create-role-button]').contains('Create Role').click();
      cy.get('app-infra-roles chef-modal').should('exist');
      cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
      cy.get('[data-cy=role-description]').type(roleDescription);

      cy.get('.navbar').contains('Run List').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('audit').click();
      cy.get('[data-cy=drag-right]').click();

      cy.get('.navbar').contains('Default Attributes').click();
      cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('.navbar').contains('Override Attributes').click();
      cy.get('[data-cy=role-override-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=add-override-attribute-button]').click();

      cy.get('app-infra-roles chef-modal chef-error').contains('already exists')
        .should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-roles chef-modal').should('exist');

      cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-roles  chef-modal').should('not.be.visible');
    });

    // delete role spec
    it('can delete a role', () => {
      cy.get('#search-filter').type(roleName);
      cy.get('[data-cy=search-role]').click();
      cy.get('#roles-table-container').contains(roleName).should('exist');

      cy.get('app-infra-roles #roles-table-container chef-td a').contains(roleName).parent().parent()
        .find('.mat-select-trigger').as('controlMenu');

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

      cy.get('#roles-table-container chef-th').should('not.be.visible');
      cy.get('[data-cy=empty-list]').should('be.visible');
      cy.get('[data-cy=no-records]').contains('No results');
      
      cy.get('#search-filter').clear();
      cy.get('[data-cy=search-role]').click();
    });
  });
});
