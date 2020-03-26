// ***********************************************************
// This example support/index.js is processed and
// loaded automatically before your test files.
//
// This is a great place to put global configuration and
// behavior that modifies Cypress.
//
// You can change the location of this file or turn off
// automatically serving support files with the
// 'supportFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/configuration
// ***********************************************************

// Import commands.js using ES2015 syntax:
import './commands';
import './constants';

// Alternatively you can use CommonJS syntax:
// require('./commands')

before(function () {
  if (!Cypress.env('RUN_FLAKY')) {
    // tslint:disable-next-line:no-string-throw
    throw new Error('MISSING ENVIRONMENT VARIABLE: You must pass CYPRESS_RUN_FLAKY.' +
      'Must be one of: "yes", "no".');
  }

  if (Cypress.env('RUN_FLAKY') !== 'yes' && Cypress.env('RUN_FLAKY') !== 'no') {
    // tslint:disable-next-line:no-string-throw
    throw new Error('INCORRECT ENVIRONMENT VARIABLE: You must pass CYPRESS_RUN_FLAKY. ' +
      'Must be one of: "yes", "no".' +
      `You passed: ${Cypress.env('RUN_FLAKY')}`);
  }

    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));

      if (!Cypress.env('ADMIN_TOKEN')) {
        cy.generateAdminToken(admin.id_token);
      }
    });

    // reset test state
    // each UI test logs in at the beginning, so we don't want any session data lying around
    cy.clearLocalStorage();
});
