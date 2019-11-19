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
import '../integration/constants';

// Alternatively you can use CommonJS syntax:
// require('./commands')

const IAMV2 = 'v2.1';
const IAMV1 = 'v1';

before(function () {
  if (!Cypress.env('IAM_VERSION')) {
    // tslint:disable-next-line:no-string-throw
    throw new Error('MISSING ENVIRONMENT VARIABLE: ' +
      `You must pass CYPRESS_IAM_VERSION. Must be one of: "${IAMV2}", "${IAMV1}".`);
  }

  if (Cypress.env('IAM_VERSION') !== IAMV2 && Cypress.env('IAM_VERSION') !== IAMV1) {
    // tslint:disable-next-line:no-string-throw
    throw new Error('INCORRECT ENVIRONMENT VARIABLE: ' +
      `You must pass CYPRESS_IAM_VERSION. Must be one of: "${IAMV2}", "${IAMV1}". ` +
      `You passed: ${Cypress.env('IAM_VERSION')}`);
  }

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

      // this needs to use the id_token because generateAdminToken will
      // throw a confusing error if you are on the wrong version of IAM so we need
      // to do this first.
      cy.request({
        auth: { bearer: admin.id_token },
        url: '/apis/iam/v2beta/policy_version'
      }).then((response) => {
        if (Cypress.env('IAM_VERSION') === IAMV2) {
          expect(response.body.version.major).to.equal('V2');
        } else {
          expect(response.body.version.major).to.equal('V1');
        }
      });

      if (!Cypress.env('ADMIN_TOKEN')) {
        cy.generateAdminToken(admin.id_token);
      }
    });

    // reset test state
    // each UI test logs in at the beginning, so we don't want any session data lying around
    cy.clearLocalStorage();
});
