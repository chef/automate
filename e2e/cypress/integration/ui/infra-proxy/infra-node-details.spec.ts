describe('infra node detail', () => {
  let adminIdToken = '';
  let nodeName = '';
  let runlist = '';

  const environment = '';
  const serverID = 'chef-manage';
  const serverName = 'chef manage';
  const orgID = 'viveksingh_msys';
  const orgName = 'viveksingh_msys';
  const serverFQDN = 'api.chef.io';
  const serverIP = '50.21.221.24';
  const adminUser = 'viveksingh_msys';
  const adminKey = Cypress.env('AUTOMATE_INFRA_ADMIN_KEY').replace(/\\n/g, '\n');
  const nullJson = '{}';
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

  function checkNodesResponse(response: any) {
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
      nodeName = response.body.nodes[0].name;
      return true;
    }
  }

  function getRunlist(name: string) {
    return cy.request({
      auth: { bearer: adminIdToken },
      failOnStatusCode: false,
      method: 'GET',
      url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/node/${nodeName}/runlist/${name}`
    });
  }

  function checkResponse(response: any) {
    if (response.status === 200) {
      if (response.body.run_list.length === 0) {
        cy.get('[data-cy=empty-runlist]').scrollIntoView().should('be.visible');
      } else {
        cy.get('.details-tab').scrollIntoView();
        cy.get('[data-cy=node-expand-runlist]').contains('Expand All');
        cy.get('[data-cy=node-collapse-runlist]').contains('Collapse All');
        cy.get('[data-cy=node-edit-runlist]').contains('Edit');
        cy.get('[data-cy=runlist-table-container] th').contains('Version');
        cy.get('[data-cy=runlist-table-container] th').contains('Position');
        runlist = response.body.run_list[0].name;
        return true;
      }
    } else {
      cy.get('[data-cy=error-runlist]').scrollIntoView().should('be.visible');
    }
  }

  describe('infra node list page', () => {
    it('displays org details', () => {
      cy.get('.page-title').contains(orgName);
    });

    // node tabs specs
    it('can switch to node tab', () => {
      cy.get('.nav-tab').contains('Nodes').click();
    });

    it('can check if node has list or not', () => {
      getNodes('', 1).then((response) => {
        checkNodesResponse(response);
      });
    });

    it('can go to details page', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=nodes-table-container] chef-td').contains(nodeName).click();
      }
    });

    // details tab specs
    it('displays infra node details', () => {
      if (nodeName !== '') {
        cy.get('.page-title').contains(nodeName);
        cy.get('[data-cy=infra-node-head]').contains(nodeName);
        cy.get('[data-cy=node-server]').contains(serverID);
        cy.get('[data-cy=node-org]').contains(orgID);
      }
    });

    it('can add the node tags', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=add-tag]').clear().type('tag1');
        cy.get('[data-cy=add-tag-button]').click();

        cy.get('app-notification.info').contains('Successfully updated node tags.');
        cy.get('app-notification.info chef-icon').click();
        cy.get('[data-cy=tag-box]').scrollIntoView();
        cy.get('[data-cy=tag-box]').should(('be.visible'));
      }
    });

    it('can show the node tags box', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=tag-box]').scrollIntoView();
        cy.get('[data-cy=tag-box]').should(('be.visible'));
      }
    });

    it('can add the multiple node tags', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=add-tag]').clear().type('tag2, tag3, tag4, tag5');
        cy.get('[data-cy=add-tag-button]').click();

        cy.get('app-notification.info').contains('Successfully updated node tags.');
        cy.get('app-notification.info chef-icon').click();
        cy.get('[data-cy=tag-box]').scrollIntoView();
        cy.get('[data-cy=tag-box]').should(('be.visible'));
        cy.get('.display-node-tags').find('span').should('have.length', 5);
      }
    });

    it('can remove the node tags', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=remove-tag]').eq(0).click();

        cy.get('app-notification.info').contains('Successfully updated node tags.');
        cy.get('[data-cy=tag-box]').scrollIntoView();
        cy.get('[data-cy=tag-box]').should(('be.visible'));
        cy.get('.display-node-tags').find('span').should('have.length', 4);
      }
    });

    it('can check environments list available or not in dropdown', () => {
      if (nodeName !== '') {
        cy.request({
          auth: { bearer: adminIdToken },
          method: 'GET',
          url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/environments`
        }).then((environmentResponse) => {
          if (environmentResponse.status === 200 && environmentResponse.body.ok === true) {
            expect(environmentResponse.status).to.equal(200);
          }
          if (environmentResponse.body.environments.length === 0) {
            cy.get('.ng-dropdown-panel-items').should('not.be.visible');
          } else {
            cy.get('.ng-arrow-wrapper').click();
            cy.get('.ng-dropdown-panel-items').should(('be.visible'));
            cy.get('.ng-arrow-wrapper').click();
          }
        });
      }
    });

    it('can select environment and display the confirmation box', () => {
      if (nodeName !== '') {
        cy.get('.ng-arrow-wrapper').click();
        cy.get('.ng-dropdown-panel-items').should(('be.visible'));
        cy.get('.ng-option').contains('google').then(option => {
          option[0].click();  // this is jquery click() not cypress click()
        });
        cy.get('[data-cy=change-confirm]').should(('be.visible'));
      }
    });

    it('can cancel the environemnt update', () => {
      if (nodeName !== '') {
        cy.get('.ng-arrow-wrapper').click();
        cy.get('.ng-dropdown-panel-items').should(('be.visible'));
        cy.get('.ng-option').contains('_default').then(option => {
          option[0].click();  // this is jquery click() not cypress click()
        });
        cy.get('[data-cy=change-confirm]').should(('be.visible'));
        cy.get('#button-env [data-cy=cancel-button]').click();
        cy.get('[data-cy=change-confirm]').should(('not.be.visible'));
      }
    });

    it('can update the environemnt', () => {
      if (nodeName !== '') {
        cy.get('.ng-arrow-wrapper').click();
        cy.get('.ng-dropdown-panel-items').should(('be.visible'));
        cy.get('.ng-option').contains('chef-environment-609823800').then(option => {
          option[0].click();  // this is jquery click() not cypress click()
        });
        cy.get('.ng-arrow-wrapper').click();
        cy.get('.ng-dropdown-panel-items').should(('be.visible'));
        cy.get('.ng-option').contains('_default').then(option => {
          option[0].click();  // this is jquery click() not cypress click()
        });
        cy.get('[data-cy=change-confirm]').should(('be.visible'));
        cy.get('[data-cy=save-button]').click();
      }
    });
  });

  describe('inside run list tab ', () => {

    // switch to Run list tab specs
    it('can switch to run list tab', () => {
      if (nodeName !== '') {
        cy.get('[data-cy=run-list-tab]').contains('Run list').click();
        cy.get('.default').contains('Run List');
      }
    });

    it('can check if node has run list or not', () => {
      if (nodeName !== '' && environment !== '' && environment === '_default') {
        getRunlist(environment).then((response) => {
          checkResponse(response);
        });
      }
    });

    it('can select multiple item from list, move to right then update the run list', () => {
      if (nodeName !== '' && runlist !== '') {
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
      }
    });

    it('can select a item from selected run list, move to left then update the run list', () => {
      if (nodeName !== '' && runlist !== '') {
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
      }
    });

    it('can check save button is disabled until run list value is not changed', () => {
      if (nodeName !== '' && runlist !== '') {
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
      }
    });

    it('can cancel edit run list', () => {
      if (nodeName !== '' && runlist !== '') {
        cy.get('[data-cy=node-edit-runlist]').contains('Edit').click();
        cy.get('app-infra-node-details chef-modal').should('exist');
        cy.get('[data-cy=drag-right]').should('be.disabled');
        cy.get('[data-cy=drag-left]').should('be.disabled');
        cy.get('[data-cy=drag-up]').should('be.disabled');
        cy.get('[data-cy=drag-down]').should('be.disabled');

        // here we exit with the Cancel button
        cy.get('[data-cy=cancel-button]').contains('Cancel').should('be.visible').click();
        cy.get('app-infra-node-details  chef-modal').should('not.be.visible');
      }
    });

    // switch to Run list tab specs
    it('can switch to details tab', () => {
      if (nodeName !== '' && runlist !== '') {
        cy.get('[data-cy=details-tab]').contains('Details').click();
      }
    });

    it('can update the environemnt and check run list updated or not', () => {
      if (nodeName !== '' && runlist !== '') {

        // can change the environment
        cy.get('.ng-arrow-wrapper').click();
        cy.get('.ng-dropdown-panel-items').should(('be.visible'));

        cy.get('.scrollable-content .ng-option').contains('chef-environment-5').click();
        cy.get('[data-cy=change-confirm]').should(('be.visible'));
        cy.get('[data-cy=save-button]').click();
        cy.get('.ng-value').contains('chef-environment-5');

        cy.get('app-notification.info').contains('Successfully updated node environment.');
        cy.get('app-notification.info chef-icon').click();

        // can switch to Run list tab specs and check run list
        cy.get('[data-cy=run-list-tab]').contains('Run list').click();
        cy.get('.default').contains('Run List');

        getRunlist(environment).then((response) => {
          checkResponse(response);
        });
      }
    });
  });

  describe('inside attributes tab ', () => {

    function updateAttributes(attribute: string) {
      const nodeAttribute = {
        org_id: orgID,
        server_id: serverID,
        name: nodeName,
        attributes: JSON.parse(attribute.replace(/\r?\n|\r/g, ''))
      };

      return cy.request({
        auth: { bearer: adminIdToken },
        method: 'PUT',
        url: `/api/v0/infra/servers/${serverID}/orgs/${orgID}/nodes/${nodeName}/attributes`,
        body: nodeAttribute
      });
    }

    // switch to attributes tab specs
    it('can switch to attributes tab', () => {
      cy.get('[data-cy=attributes-tab]').contains('Attributes').click();
      cy.get('.default').contains('Attributes');
    });

    it('attribute tab contains buttons', () => {
      cy.get('.default').contains('Attributes');
      cy.get('[data-cy=expand-attributes]').contains('Expand All');
      cy.get('[data-cy=collapse-attributes]').contains('Collapse All');
      cy.get('[data-cy=node-edit-attributes]').contains('Edit');
    });

    it('can check save button is disabled until attributes value is not changed', () => {
      cy.get('[data-cy=node-edit-attributes]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');

      // check for disabled save button
      cy.get('[data-cy=save-attribute]')
      .invoke('attr', 'disabled')
      .then(disabled => {
        disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=save-attribute]').click();
      });

      cy.get('app-infra-node-details chef-modal').should('exist');

      // here we exit with the Cancel button
      cy.get('[data-cy=cancel-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-node-details  chef-modal').should('not.be.visible');
    });

    xit('edit attribute and show empty data', () => {
      cy.get('[data-cy=node-edit-attributes]').contains('Edit').click({force: true});
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=attributes]').clear().invoke('val', nullJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=save-attribute]').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      updateAttributes(nullJson).then((response) => {
        if (Object.keys(response.body.attributes).length > 0) {
          cy.get('[data-cy=expand-attributes]').should('not.be.disabled');
          cy.get('[data-cy=collapse-attributes]').should('not.be.disabled');
        } else {
          cy.get('[data-cy=expand-attributes]').should('be.disabled');
          cy.get('[data-cy=collapse-attributes]').should('be.visible');
        }
      });
    });

    xit('edit attribute and show updated data', () => {
      cy.get('[data-cy=node-edit-attributes]').contains('Edit').click({force: true});
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=attributes]').clear().invoke('val', validJson)
        .trigger('change').type(' ');

      cy.get('[data-cy=save-attribute]').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');

      // verify success notification and then dismiss it
      // so it doesn't get in the way of subsequent interactions
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();

      updateAttributes(validJson).then((response) => {
        if (Object.keys(response.body.attributes).length > 0) {
          cy.get('[data-cy=expand-attributes]').should('not.be.disabled');
          cy.get('[data-cy=collapse-attributes]').should('not.be.disabled');
        } else {
          cy.get('[data-cy=expand-attributes]').should('be.disabled');
          cy.get('[data-cy=collapse-attributes]').should('be.visible');
        }
      });
    });

    xit('fails to edit attribute with a invalid json', () => {
      cy.get('[data-cy=node-edit-attributes').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=attributes]').clear().invoke('val', invalidJson).trigger('change');

      cy.get('app-infra-node-details chef-modal chef-error')
        .contains('Must be a valid JSON object').should('be.visible');
      cy.get('[data-cy=cancel-attribute-button]').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');
    });

    xit('can cancel edit attributes', () => {
      cy.get('[data-cy=node-edit-attributes]').contains('Edit').click();
      cy.get('app-infra-node-details chef-modal').should('exist');
      cy.get('[data-cy=cancel-attribute-button]').contains('Cancel').should('be.visible').click();
      cy.get('app-infra-node-details chef-modal').should('not.be.visible');
    });

    // switch to Run details tab
    it('can switch to details tab', () => {
      cy.get('[data-cy=details-tab]').contains('Details').click();
    });
  });
});
