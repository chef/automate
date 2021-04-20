describe('infra role detail', () => {
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
  const adminKey = `
MIIEpQIBAAKCAQEA6KIxELz/HWjwT8qiQrhPbvymVG/hnF/n8owZfh04hsABneB8
u6xklW+VjCRMBHfptovHn+5NyN5blR1wpXQc5iKXEPfDny/gncNAeu4lrezs8f8z
dZa2jFOwFH/Cm4o1+po3uhXtAsdtN/WI7EiCHZEeXRA5jFYF86Y92G0FDoaRiA4G
jWu7M3hL/61ELZ80d2RH+GdKgFqQh9hRlnx59ozmMW4maaMXpx2eb8RLvYgKBAUH
5qGpWdMcODsLBkIy/+RqpLdsMFxrvFWBeTVBQvecx/UZMMWU3yQigzRismiYuLug
nbN7DhgK71GXPkHG4JoxltgW5E+lYtjZD1wh9wIDAQABAoIBAQCJSGWyHgZjQbFH
NSqKOyBNO/WgMKIwWPyVSw4kOXRJOPf7RiX1zqdQ9JeJK0ZdALLAUj7M56Gpn2bm
WYhHa30+Zj1F+yDLSULBdx8PLIi52e5+ZP7mLrmtmBl6D2c1yNtP90BZpWTH1g5j
Dpft8GAwuJn1i4Sah41dmsY2eSeZyLn1RMj8MXW/bXr4QbRtvxjXcW6Sfstj8jrV
zjG/kbptPzoFTwqUcIR8xmmlewSqOERMhdJXYNW07uVMzmZTKB/UrJQr+lzx4otJ
g0uHQftcku00/KW6iyGTKbQh9gm4UJ4JfVRg9rrCL2NPvLw6Y3BzlKTVOxj8UFI5
yoZ/YBLpAoGBAPjwJQptYtgauW6MR7wboa4YK4ltB9Qa7SIbfVGPQISDhfdpyg/6
LZW65MwHPbWM4NjwwpkaLuBrBY0eqGvxtdWAvLPCBTRfoSay5v9E7qFgWWTNWhuq
C/SuzJeh1bnSNoNW0/fKDDAPsU+x3gUNKiu0VWFCq5tTk78NWat3wpLDAoGBAO87
o6csdMSrLOaqoUCqUOJNpv6h8uMxLyOPJWgqAIZCVNpZaPOuWseHavkofkI5Hpzu
T1rBEZZKVjZN8JFXc+1O4n9uqd8kXooONqsRYDDPcKxHqvibXWl42PeNDx/tcETo
dXiwhjtHuoBjKTyp18+ND8i8wjIb0pEwjLnPBpi9AoGBAKRnZO8YAtmZ/LEHsDCJ
RyQFDmu6OBtalRWbdg4xP3jEq+sUUMBwhz6FqakddyG5op1jlAEtC5xm0cm7X5u6
jE4usXE+R9Wo+nLmYCxiR/8H9MHf5Qy0JyU3dCMIXHBWZm4jH+Zb5+St2Ho7i9dm
NF2cwW50a4UvkznwBTJ662OlAoGAU4fsbXutTdpC9EuRwEV3tnzMmU4sw70xq2XY
9tTrl/VjMVZUufkgA1k6NcM4yqCtGrhvBs4w+Nv93Do8jFMPzEVl+n7GW36Ub/nk
hrircH+N5OmlPebpp+ElSNJ8/HXoZHcSRVDFnb8+1INLK75V90dWwo199QcX79AW
4u3xbLUCgYEAm+1Dv8bvC9d3Z08mCJjUbzdRG6qA39EXpixVYjbmXmDpy71KA2zR
LvgdoNIAiVKFUcR1z8aty8HNJKzzZPL35VpFJ5Sm4Zh99OVDJkRxpWdZvqdL865h
8/A/e8ZFjAWF8m83OlP0sb1dn8CQ8Pf+hFfW/a97Y7maECqU0oyNXJg=
`;
  const roleName = `${cypressPrefix}-role-${now}`;
  const roleDescription = 'role description';
  const defaultAttribute = {default: 'test'};
  const overrideAttribute = {override: 'test'};
  const runlist = ['recipe[aix::nim_master_setup]', 'recipe[aix::nim_master_setup_standalone]'];
  const validJson = '{"test":"test"}';
  const invalidJson = '{"invalid "test"';
  const nestedJson = '{"id":"0001","batters":{"batter":[{"id":"1001"},{"id": "1002"}]} }';

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
          admin_key: `-----BEGIN RSA PRIVATE KEY-----${adminKey}-----END RSA PRIVATE KEY-----`
        }
      }).then((resp) => {
        if (resp.status === 200 && resp.body.ok === true) {
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
      }).then((resp) => {
        if (resp.status === 200 && resp.body.ok === true) {
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
      cy.get('.version-dropdown .selected-value .option-content').contains('_default')
        .should('exist');

      cy.get('.version-dropdown .selected-value').contains('_default').click();
      cy.get('.version-dropdown .options .option-content').contains('_default').click();
      cy.get('.version-dropdown .selected-value').contains('_default').should('exist');
      cy.wait(2000);
    });

    it('can search a item from list', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('[data-cy=search-roles-and-recipes]').type('aix');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('aix')
        .should('exist');
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').clear();
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').type('test');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('test')
        .should('exist');
      cy.wait(1000);
      cy.get('[data-cy=search-roles-and-recipes]').clear();
      cy.wait(2000);
      cy.get('[data-cy=search-roles-and-recipes]').type('exp');
      cy.get('.cdk-virtual-scroll-content-wrapper .no-data').contains('Not Available')
        .should('exist');
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
    it('can select available roles and recipes, only roles, only recipes to load respective data',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.select-box .mat-select .mat-select-value-text')
        .contains('available roles and recipes').should('exist');

      cy.get('.select-box .mat-select ').contains('available roles and recipes').click();
      cy.get('.cdk-overlay-container .mat-option .mat-option-text [alt="Available roles"]').click();
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
      cy.get('app-infra-role-detailss  chef-modal').should('not.be.visible');

    });

    it('can select a item from list, move to right then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('chef-client')
        .click();

      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.wait(3000);
      cy.get('#runlist-table-container td #run-list-name').contains('chef-client').should('exist');
    });

    it('can select multiple item from list, move to right then update the runlist', () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list')
        .contains('centos-cookbook-file').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('cron').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('chef-sugar').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('logrotate').click();
      cy.get('.cdk-virtual-scroll-content-wrapper #select-run-list').contains('openldap').click();

      cy.get('[data-cy=drag-right]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(3000);
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file')
        .should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('cron').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('chef-sugar').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('logrotate').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('openldap').should('exist');
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

    it('can select multiple item from selected run list, move to left then update the runlist',
      () => {
      cy.get('[data-cy=edit-runlist]').contains('Edit').click();
      cy.get('app-infra-role-details chef-modal').should('exist');
      cy.get('.vertical #updated-run-list').contains('chef-sugar').click();
      cy.get('.vertical #updated-run-list').contains('openldap').click();

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
      cy.get('#runlist-table-container td #run-list-name')
        .contains('aix::nim_master_setup_standalone').should('exist');
    });

    it('can select multiple item from selected run list, move item up then update the runlist',
      () => {
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
      cy.get('#runlist-table-container td #run-list-name')
        .contains('aix::nim_master_setup_standalone').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file')
        .should('exist');
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
      cy.get('.vertical #updated-run-list').contains('centos-cookbook-file').click();

      cy.get('[data-cy=drag-down]').click();
      cy.get('[data-cy=update-run-list]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      cy.wait(2000);
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file')
        .should('exist');
    });

    it('can select multiple item from selected run list, move item down then update the runlist',
      () => {
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

      cy.get('#runlist-table-container td #run-list-name')
        .contains('aix::nim_master_setup_standalone').should('exist');
      cy.get('#runlist-table-container td #run-list-name').contains('centos-cookbook-file')
        .should('exist');
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
      cy.get('[data-cy=default-attribute]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=update-default-attribute]').click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.wait(2000);
      cy.get('app-notification.info chef-icon').click();
      cy.wait(2000);
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
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=update-default-attribute]').click();
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

      cy.get('app-infra-role-details chef-modal chef-error').contains('Must be a valid JSON object')
      .should('be.visible');

      //  here we exit with the chef-modal exit button in the top right corner
      cy.get('app-infra-role-details chef-modal chef-button.close').first().click();
      cy.get('app-infra-role-details chef-modal').should('not.be.visible');
    });

    it('edit override attribute', () => {
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
      cy.wait(2000);
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
      cy.get('app-notification.info chef-icon').click({multiple: true});
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

      cy.get('app-infra-role-details chef-modal chef-error').contains('Must be a valid JSON object')
      .should('be.visible');

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
      cy.get('#search-filter').type(roleName);
      cy.get('[data-cy=search-role]').click();

      cy.get('#roles-table-container').contains(roleName).should('exist');
      cy.get('app-infra-roles #roles-table-container chef-td a').contains(roleName).parent()
        .parent().find('.mat-select-trigger').as('controlMenu');
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

      cy.get('#search-filter').clear();
      cy.get('[data-cy=search-role]').click();
    });
  });
});
