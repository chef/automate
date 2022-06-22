describe('infra role', () => {
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
  const roleName = `${cypressPrefix}-role-${now}-1`;
  const roleDescription = 'role description';
  const roleRunlistName = `${cypressPrefix}-role-${now}-2`;
  const roleDefaultAttrName = `${cypressPrefix}-role-${now}-3`;
  const roleOverrideAttrName = `${cypressPrefix}-role-${now}-4`;
  const validJson = '{"test":"test"}';
  const invalidJson = '{"invalid "test"';

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

  function getRole(role: string, page: number, per_page = 9) {
    const wildCardSearch = '*';
    const target = roleName !== '' ?
    'name:' + wildCardSearch + role : wildCardSearch + ':';
    const nameTarget = target + wildCardSearch;
    const currentPage = page - 1;
    // Add asterisk to do wildcard search
    const params =
  `search_query.q=${nameTarget}&search_query.page=${currentPage}&search_query.per_page=${per_page}`;
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/roles?${params}`
    });
  }

  function checkResponse(response: any) {
    if (response.body.roles.length === 0) {
      cy.get('[data-cy=empty-list]').should('be.visible');
    } else {
      cy.get('[data-cy=create-role-button]').contains('Create Role');
      cy.get('[data-cy=roles-table-container] chef-th').contains('Name');
      cy.get('[data-cy=roles-table-container] chef-th').contains('Description');
      cy.get('[data-cy=roles-table-container] chef-th').contains('Environments');
      return true;
    }
  }

  describe('infra role list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // roles tabs specs
    it('can switch to roles tab', () => {
      cy.get('.nav-tab').contains('Roles').click();
    });

    it('can check if role has list or not', () => {
      getRole('', 1).then((response) => {
        checkResponse(response);
      });
    });

    context('can search and change page in role', () => {
      it('can search a role and check  if empty or not', () => {
        cy.get('[data-cy=search-filter]').type(roleName);
        cy.get('[data-cy=search-entity]').click();
        getRole(roleName, 1).then((response) => {
          checkResponse(response);
        });
        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getRole('', 1).then((response) => {
          checkResponse(response);
        });
      });

      xit('can change page and load data according to page', () => {
        const page = 3;
        cy.get('[data-cy=roles-table-container] chef-th').contains('Name');
        cy.get('[data-cy=roles-table-container] chef-th').contains('Description');
        cy.get('[data-cy=roles-table-container] chef-th').contains('Environments');
        if (cy.get('.roles-list-paging .page-picker-item').contains('3')) {
          cy.get('.roles-list-paging .page-picker-item').contains('3').click();
          getRole('', page).then((response) => {
            checkResponse(response);
          });
        }
      });
    });

    // In create role pop-up details tab specs
    context('can create role with details tab', () => {
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
    });

    // In create role pop-up run list tab specs
    context('can create role with run list tab', () => {
      it('can add a name, description, run list in role', () => {
        cy.wait(1000);
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleRunlistName);
        cy.get('[data-cy=role-description]').type(roleDescription);

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('audit')
          .click();

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
        cy.get('[data-cy=navbar]').contains('Run List').click();

        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-run-list-button]').contains('Cancel').should('be.visible')
          .click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });

      it('can check create run list button is disabled until all inputs are filled in', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleRunlistName);
        cy.get('[data-cy=role-description]').type(roleDescription);
        cy.get('[data-cy=navbar]').contains('Run List').click();

        // check for disabled
        cy.get('[data-cy=add-run-list-button]')
        .invoke('attr', 'disabled')
        .then(disabled => {
          disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-run-list-button]').click();
        });

        cy.get('app-infra-roles chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-run-list-button]').contains('Cancel').should('be.visible')
          .click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });
    });

    // In create role pop-up default attribute tab specs
    context('can create role with default attribute tab', () => {
      it('can add a name, description, run list and default attribute in role', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('audit')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
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

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').focus();
        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });

      it('can check create default attribute button is disabled until textarea is filled', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);
        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear();

        // check for disabled
        cy.get('[data-cy=add-default-attribute-button]')
        .invoke('attr', 'disabled')
        .then(disabled => {
          disabled ? cy.log('buttonIsDiabled') :
            cy.get('[data-cy=add-default-attribute-button]').click();
        });

        cy.get('app-infra-roles chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });

      it('failed to create default attribute with invalid json', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleDefaultAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);
        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', invalidJson)
          .trigger('change');

        cy.get('app-infra-roles chef-modal chef-error').contains('Must be a valid JSON object')
          .should('be.visible');

        cy.wait(1000);
        cy.get('app-infra-roles chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-default-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });
    });

    // In create role pop-up override attribute tab specs
    context('can create role with override attribute tab', () => {
      it('can add a name, description, run list, default attribute and override attribute in role',
        () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('audit')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
          .trigger('change').type(' ');

        cy.get('[data-cy=navbar]').contains('Override Attributes').click();
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

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
          .trigger('change').type(' ');

        cy.get('[data-cy=navbar]').contains('Override Attributes').click();
        cy.get('[data-cy=role-override-attribute]').focus();

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });

      it('can check create override attribute button is disabled until textarea is filled', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);
        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
          .trigger('change').type(' ');

        cy.get('[data-cy=navbar]').contains('Override Attributes').click();
        cy.get('[data-cy=role-override-attribute]').clear();

        // check for disabled
        cy.get('[data-cy=add-override-attribute-button]')
        .invoke('attr', 'disabled')
        .then(disabled => {
          disabled ? cy.log('buttonIsDiabled') :
            cy.get('[data-cy=add-override-attribute-button]').click();
        });

        cy.get('app-infra-roles chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });

      it('failed to create override attribute with invalid json', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);
        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]').contains('aix')
          .click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
          .trigger('change').type(' ');

        cy.get('[data-cy=navbar]').contains('Override Attributes').click();
        cy.get('[data-cy=role-override-attribute]').invoke('val', invalidJson)
          .trigger('change').type(' ');

        cy.get('app-infra-roles chef-modal chef-error').contains('Must be a valid JSON object')
          .should('be.visible');

        cy.wait(1000);
        cy.get('app-infra-roles chef-modal').should('exist');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-override-attribute-button]').contains('Cancel')
          .should('be.visible').click();
        cy.get('app-infra-roles  chef-modal').should('not.be.visible');
      });
    });

    context('failed to create role with dublicate name', () => {
      it('can move to details tab if fails to create a role with a duplicate name', () => {
        cy.get('[data-cy=create-role-button]').contains('Create Role').click();
        cy.get('app-infra-roles chef-modal').should('exist');
        cy.get('[data-cy=role-name]').type(roleOverrideAttrName);
        cy.get('[data-cy=role-description]').type(roleDescription);

        cy.get('[data-cy=navbar]').contains('Run List').click();
        cy.get('.cdk-virtual-scroll-content-wrapper [data-cy=select-run-list]')
          .contains('audit').click();
        cy.get('[data-cy=drag-right]').click();

        cy.get('[data-cy=navbar]').contains('Default Attributes').click();
        cy.get('[data-cy=role-deffault-attribute]').clear().invoke('val', validJson)
          .trigger('change').type(' ');

        cy.get('[data-cy=navbar]').contains('Override Attributes').click();
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
    });

    // delete role spec
    context('can delete roles', () => {
      it('can delete multiple roles', () => {
        cy.get('[data-cy=search-filter]').type(`${cypressPrefix}-role-${now}`);
        cy.get('[data-cy=search-entity]').click();
        getRole(`${cypressPrefix}-role-${now}`, 1).then((response) => {
          if (checkResponse(response)) {
            cy.get('[data-cy=roles-table-container]').contains(roleName).should('exist');
            cy.get('app-infra-roles [data-cy=roles-table-container] chef-td a')
              .contains(roleName).parent().parent().find('.mat-select-trigger').click();
            // we throw in a `should` so cypress retries until introspection allows
            // menu to be shown

            cy.get('[data-cy=delete-role]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-infra-roles chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info').contains(`Successfully deleted role - ${roleName}.`);
            cy.get('app-notification.info chef-icon').click();
          }
        });

        getRole(`${cypressPrefix}-role-${now}`, 1).then((response) => {
          if (checkResponse(response)) {
            cy.wait(2000);
            cy.get('[data-cy=roles-table-container]').contains(roleRunlistName).should('exist');
            cy.get('app-infra-roles [data-cy=roles-table-container] chef-td a')
              .contains(roleRunlistName).parent().parent().find('.mat-select-trigger')
              .as('controlMenu');
            // we throw in a `should` so cypress retries until introspection allows
            // menu to be shown
            cy.get('@controlMenu').scrollIntoView().should('be.visible')
              .click();
            cy.get('[data-cy=delete-role]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-infra-roles chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info')
              .contains(`Successfully deleted role - ${roleRunlistName}.`);
            cy.get('app-notification.info chef-icon').click();
          }
        });

        getRole(`${cypressPrefix}-role-${now}`, 1).then((response) => {
          if (checkResponse(response)) {
            cy.wait(2000);
            cy.get('[data-cy=roles-table-container]').contains(roleDefaultAttrName)
              .should('exist');
            cy.get('app-infra-roles [data-cy=roles-table-container] chef-td a')
              .contains(roleDefaultAttrName).parent().parent().find('.mat-select-trigger')
              .as('controlMenu');
            // we throw in a `should` so cypress retries until introspection allows
            // menu to be shown
            cy.get('@controlMenu').scrollIntoView().should('be.visible')
              .click();
            cy.get('[data-cy=delete-role]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-infra-roles chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info')
              .contains(`Successfully deleted role - ${roleDefaultAttrName}.`);
            cy.get('app-notification.info chef-icon').click();
          }
        });

        getRole(`${cypressPrefix}-role-${now}`, 1).then((response) => {
          if (checkResponse(response)) {
            cy.wait(2000);
            cy.get('[data-cy=roles-table-container]').contains(roleOverrideAttrName)
              .should('exist');
            cy.get('app-infra-roles [data-cy=roles-table-container] chef-td a')
              .contains(roleOverrideAttrName).parent().parent().find('.mat-select-trigger')
              .as('controlMenu');
            // we throw in a `should` so cypress retries until introspection
            // allows menu to be shown
            cy.get('@controlMenu').scrollIntoView().should('be.visible')
              .click();
            cy.get('[data-cy=delete-role]').should('be.visible')
              .click();
            // accept dialog
            cy.get('app-infra-roles chef-button').contains('Delete').click();
            // verify success notification and then dismiss it
            cy.get('app-notification.info')
              .contains(`Successfully deleted role - ${roleOverrideAttrName}.`);
            cy.get('app-notification.info chef-icon').click();
          }
        });

        getRole(`${cypressPrefix}-role-${now}`, 1).then((response) => {
          checkResponse(response);
        });

        cy.get('[data-cy=search-filter]').clear();
        cy.get('[data-cy=search-entity]').click();
        getRole('', 1).then((response) => {
          checkResponse(response);
        });
      });
    });
  });
});
