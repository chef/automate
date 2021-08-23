describe('chef datafeed', () => {

    const name = "cytest",
          url = "http://test.com",
          tokenType = "TestType",
          token = "behwveh3238238=";

  
    before(() => {
        cy.adminLogin('/settings').then(() => {
            const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
            cy.get('app-welcome-modal').invoke('hide');
            cy.restoreStorage();
            cy.get('body').type('feat');
            cy.get('.title').contains('Chef Automate Data Feed').parent().parent()
              .find('.onoffswitch').click();
            cy.get('chef-button').contains('Close').click();
            cy.reload();
            cy.contains('know').click();
            cy.contains('Data Feeds').click();
            cy.get('app-notification.error chef-icon').click();
        });
    });
    
    beforeEach(() => {
      cy.restoreStorage();
    });
    
    afterEach(() => {
        cy.saveStorage();
    });
    
    describe ('chef data feed page', () => {
      const reusableDate = Date.now();
  
      it('check if clicking on new integration button opens up the slider', () => {
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=interation-menu]').should('be.visible');
      });

      it('check if clicking on a interation opens the form', () => {
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=ServiceNow]').click();
        cy.get('[data-cy=data-feed-create-form]').should('be.visible');
      });

      it('create data feed', () => {
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=ServiceNow]').click();
        cy.get('[data-cy=add-name]').type(name+reusableDate);
        cy.get('[data-cy=add-url]').type(url);
        cy.get('[data-cy=add-token]').type(token);
        cy.get('[data-cy=add-button]').click();
        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();
        cy.get('chef-table chef-tbody chef-td').contains("cytest"+reusableDate).should('exist');
      });

      it('create data feed error', () => {
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=ServiceNow]').click();
        cy.get('[data-cy=add-name]').type(name+reusableDate);
        cy.get('[data-cy=add-url]').type(url);
        cy.get('[data-cy=add-token]').type(token);
        cy.get('[data-cy=add-button]').click();
        cy.get('app-notification.error').should('be.visible');
        cy.get('app-notification.error chef-icon').click();
      });

      it('create data feed with changed token type', () => {
        const date = Date.now();
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=ServiceNow]').click();
        cy.get('[data-cy=add-name]').type(name+date);
        cy.get('[data-cy=add-url]').type(url);
        cy.get('[data-cy=toggle-type]').click();
        cy.get('[data-cy=add-token-type]').clear();
        cy.get('[data-cy=add-token-type]').type(tokenType)
        cy.get('[data-cy=add-token]').type(token);
        cy.get('[data-cy=add-button]').click();
        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info chef-icon').click();
        cy.get('chef-table chef-tbody chef-td').contains("cytest"+date).should('exist');
      });

      it('test error in data feed', () => {
        const date = Date.now();
        cy.get('[data-cy=create-data-feed]').click();
        cy.get('[data-cy=ServiceNow]').click();
        cy.get('[data-cy=add-name]').type(name+date);
        cy.get('[data-cy=add-url]').type(url);
        cy.get('[data-cy=add-token]').type(token);
        cy.get('[data-cy=test-button]').click();
        cy.get('app-data-feed-create').scrollTo('top');
        cy.get('app-notification.error').should('be.visible');
        cy.get('app-notification.error chef-icon').click();
      });
    });
  });


// describe('chef server', () => {
//     const now = Cypress.moment().format('MMDDYYhhmmss');
//     const cypressPrefix = 'infra';
//     const serverName = `${cypressPrefix} server ${now}`;
//     const generatedServerID = serverName.split(' ').join('-');
//     const customServerID = `${cypressPrefix}-custom-id-${now}`;
//     const serverFQDN = 'chef-server-1617089723092818000.com';
//     const serverIP = '176.119.50.159';
  
//     before(() => {
//       cy.adminLogin('/infrastructure/chef-servers').then(() => {
//         const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
//       });
//       cy.restoreStorage();
//     });
  
//     beforeEach(() => {
//       cy.restoreStorage();
//     });
  
//     afterEach(() => {
//       cy.saveStorage();
//     });
  
//     describe('chef server list page', () => {
  
//       it('can add a infra server', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
//         cy.get('app-chef-servers-list chef-modal').should('exist');
//         cy.get('[data-cy=add-name]')
//           .should('have.focus')
//           .should('have.attr', 'firstFocus');
//         cy.get('[data-cy=add-name]').type(serverName);
//         cy.get('[data-cy=id-label]').contains(generatedServerID);
//         cy.get('[data-cy=add-fqdn]').type(serverFQDN);
//         cy.get('[data-cy=add-ip-address]').type(serverIP);
  
//         cy.get('[data-cy=add-button]').click();
//         cy.get('app-chef-servers-list chef-modal').should('not.be.visible');
  
//         // verify success notification and then dismiss it
//         // so it doesn't get in the way of subsequent interactions
//         cy.get('app-notification.info').should('be.visible');
//         cy.get('app-notification.info chef-icon').click();
  
//         cy.get('app-chef-servers-list chef-tbody chef-td').contains(serverName).should('exist');
//       });
  
//       it('lists servers', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server');
  
//         cy.get('#servers-table-container chef-th').contains('Name');
//         cy.get('#servers-table-container chef-th').contains('FQDN');
//         cy.get('#servers-table-container chef-th').contains('IP Address');
//         cy.get('#servers-table-container chef-th').contains('Number Of Orgs');
//       });
  
//       it('can create a chef server with a custom ID', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
//         cy.get('app-chef-servers-list chef-modal').should('exist');
//         cy.get('[data-cy=add-name]').type(serverName);
//         cy.get('[data-cy=add-id]').should('not.be.visible');
//         cy.get('[data-cy=edit-button]').contains('Edit ID').click();
//         cy.get('[data-cy=id-label]').should('not.be.visible');
//         cy.get('[data-cy=add-id]').should('be.visible').clear().type(customServerID);
//         cy.get('[data-cy=add-fqdn]').type(serverFQDN);
//         cy.get('[data-cy=add-ip-address]').type(serverIP);
  
//         cy.get('[data-cy=add-button]').click();
//         cy.get('app-chef-servers-list chef-modal').should('not.be.visible');
  
//         // verify success notification and then dismiss it
//         // so it doesn't get in the way of subsequent interactions
//         cy.get('app-notification.info').should('be.visible');
//         cy.get('app-notification.info chef-icon').click();
  
//         cy.get('app-chef-servers-list chef-tbody chef-td').contains(serverName).should('exist');
//       });
  
//       it('fails to create a chef server with a duplicate ID', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
//         cy.get('app-chef-servers-list chef-modal').should('exist');
//         cy.get('[data-cy=add-name]').type(serverName);
//         cy.get('[data-cy=add-id]').should('not.be.visible');
//         cy.get('[data-cy=edit-button]').contains('Edit ID').click();
//         cy.get('[data-cy=id-label]').should('not.be.visible');
//         cy.get('[data-cy=add-id]').should('be.visible').clear().type(customServerID);
//         cy.get('[data-cy=add-fqdn]').type(serverFQDN);
//         cy.get('[data-cy=add-ip-address]').type(serverIP);
//         cy.get('[data-cy=add-button]').click();
//         cy.get('app-chef-servers-list chef-modal chef-error').contains('already exists')
//           .should('be.visible');
  
//         //  here we exit with the chef-modal exit button in the top right corner
//         cy.get('app-chef-servers-list chef-modal chef-button.close').first().click();
//       });
  
//       it('can cancel creating a chef server', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
//         cy.get('app-chef-servers-list chef-modal').should('exist');
  
//         // here we exit with the Cancel button
//         cy.get('chef-button').contains('Cancel').should('be.visible').click();
//         cy.get('app-chef-servers-list  chef-modal').should('not.be.visible');
//       });
  
//       it('can delete a chef server', () => {
//         cy.get('app-chef-servers-list chef-td a').contains(serverName).parent().parent()
//           .find('.mat-select-trigger').as('controlMenu');
  
//         // we throw in a `should` so cypress retries until introspection allows menu to be shown
//         cy.get('@controlMenu').scrollIntoView().should('be.visible')
//           .click();
//         cy.get('[data-cy=delete-server]').should('be.visible')
//           .click();
  
//         // accept dialog
//         cy.get('app-chef-servers-list chef-button').contains('Delete').click();
  
//         // verify success notification and then dismiss it
//         cy.get('app-notification.info').contains('Successfully deleted server');
//         cy.get('app-notification.info chef-icon').click();
  
//         cy.get('app-chef-servers-list chef-tbody chef-td')
//           .contains(customServerID).should('not.exist');
//       });
  
//       it('can check create server button is disabled until all inputs are filled in', () => {
//         cy.get('[data-cy=add-server-button]').contains('Add Chef Infra Server').click();
//         cy.get('app-chef-servers-list chef-modal').should('exist');
//         cy.get('[data-cy=add-name]').type(serverName);
//         cy.get('[data-cy=id-label]').contains(generatedServerID);
//         cy.get('[data-cy=add-fqdn]').type(serverFQDN);
  
//         // check for disabled
//         cy.get('[data-cy=add-button]')
//         .invoke('attr', 'disabled')
//         .then(disabled => {
//           disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=add-button]').click();
//         });
  
//         cy.get('app-chef-servers-list chef-modal').should('exist');
  
//         // here we exit with the Cancel button
//         cy.get('chef-button').contains('Cancel').should('be.visible').click();
//         cy.get('app-chef-servers-list  chef-modal').should('not.be.visible');
//       });
//     });
//   });
  