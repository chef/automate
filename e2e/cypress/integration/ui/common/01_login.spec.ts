if (Cypress.env('SKIP_SSO')) {
  describe('SSO', () => {
    it.skip('is disabled');
  });
} else {
  describe('login and logout', () => {

    describe('presents SSO login capabilities', () => {
      before(() => {
        cy.visit('/');
      });

      it('greets with SSO page', () => {
        cy.url()
          .should('include', '/dex/auth?')
          .should('include', 'client_id=automate-session')
          .should('include', 'redirect_uri');
        cy.contains('Sign in as a local user');
        cy.contains('Sign in with SAML');
      });

      it('selecting username sign-on transits to local page', () => {
        cy.contains('Sign in as a local user').click({ force: true }).then(() => {
          cy.url().should('include', '/dex/auth/');
          cy.contains('Username');
          cy.contains('Password');
          cy.contains('Sign In');
        });
      });

      it('back button returns to SSO login page', () => {
        cy.contains('Back').click().then(() => {
          cy.contains('Sign in as a local user');
          cy.contains('Sign in with SAML');
        });
      });
    });

    describe('fails to login with wrong credentials', () => {

      before(() => {
        cy.visit('/');
      });

      it('displays error with wrong credentials entered', () => {
        cy.contains('Sign in as a local user').click({ force: true }).then(() => {

          cy.get('#login').type('admin');
          cy.get('#password').type('wrong');

          cy.get('[type=submit]').click().then(() => {
            cy.contains('Username or password is incorrect.');
            cy.get('#login').clear();
            cy.get('#password').clear();
          });
        });
      });
    });

    describe('can login/logout as admin', () => {
      before(() => {
        cy.visit('/');
      });

      it('can login and welcome modal appears', () => {
        cy.contains('Sign in as a local user').click({ force: true }).then(() => {
          cy.get('#login').type('admin');
          cy.get('#password').type('chefautomate');

          cy.get('[type=submit]').click().then(() => {
            cy.get('[data-cy=welcome-title]').should('exist');
            cy.url().should('include', '/event-feed'); // default landing page
            cy.contains('Local Administrator').should('exist'); // current user name
          });
        });
      });

      it('can close modal with main button', () => {
        cy.get('[data-cy=welcome-title]').should('exist');
        cy.get('[data-cy=close-welcome]').click().then(() => {
          cy.get('[data-cy=welcome-title]').should('not.exist');
        });
      });

      it('has expected profile menu choices', () => {
        const menuChoices = [
          'Signed in as Local Administrator',
          'Profile',
          'Version',
          'Build',
          'About',
          'License',
          'Release Notes',
          'Sign Out'
        ];
        cy.get('[data-cy=user-profile-button]').click().then(() => {
          menuChoices.forEach((choice) => {
            cy.get('ul.dropdown-list li').contains(choice);
          });
          cy.get('[data-cy=user-profile-button]').click(); // close profile menu
        });
      });

      it('can log out and return to SSO page', () => {
        cy.get('[data-cy=user-profile-button]').click().then(() => {
          cy.get('[data-cy=sign-out-button]').click().then(() => {
            cy.url().should('include', '/dex/auth');
            cy.contains('Sign in as a local user');
            cy.contains('Sign in with SAML');
          });
        });
      });
    });

    describe('welcome', () => {
      it('displays welcome modal after first login', () => {
        // ensure browser is logging in for the first time
        clearAllStorage();

        // log in
        login();

        // verify welcome modal is visible
        cy.get('[data-cy=welcome-title]').should('be.visible');
      });

      it('does not display welcome modal again after page refresh', () => {
        // ensure browser is logging in for the first time and log in
        clearAllStorage();
        login();

        // verify welcome modal is visible
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // close modal and refresh page
        closeModal();
        cy.reload();
        cy.get('h1.page-title').should('be.visible');

        // verify welcome modal is not visible
        cy.get('[data-cy=welcome-title]').should('not.exist');
      });

      it('does not display welcome modal again after subsequent logins', () => {
        // ensure browser is logging in for the first time and log in
        clearAllStorage();
        login();

        // verify welcome modal is visible
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // close modal, logout, and log back in
        closeModal();
        logout();
        login();

        // verify welcome modal is not visible
        cy.get('[data-cy=welcome-title]').should('not.exist');
      });

      it('provides option to always display welcome modal after login', () => {
        // ensure logged in and welcome modal is visible
        cy.clearLocalStorage();
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // verify checkbox is visible and unchecked by default
        cy.get('.show-again-checkbox').should('be.visible');
        cy.get('.show-again-checkbox').should('have.attr', 'aria-checked', 'false');

        // check the checkbox
        cy.get('.show-again-checkbox').click();
        cy.get('.show-again-checkbox').should('have.attr', 'aria-checked', 'true');

        // close modal, logout, and log back in
        closeModal();
        logout();
        login();

        // verify welcome modal is always visible after login
        cy.get('[data-cy=welcome-title]').should('be.visible');
        cy.get('.show-again-checkbox').should('have.attr', 'aria-checked', 'true');

        // uncheck the checkbox
        cy.get('.show-again-checkbox').click();
        cy.get('.show-again-checkbox').should('have.attr', 'aria-checked', 'false');

        // close modal, logout, and log back in
        closeModal();
        logout();
        login();

        // verify welcome modal is no longer always visible after login
        cy.get('[data-cy=welcome-title]').should('not.be.visible');
      });

      it('provides option to toggle app telemetry', () => {
        // ensure logged in and welcome modal is visible
        clearAllStorage();
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // verify checkbox is visible and checked by default
        cy.get('app-telemetry-checkbox chef-checkbox').should('be.visible');
        cy.get('app-telemetry-checkbox chef-checkbox').should('have.attr', 'aria-checked', 'true');
      });

      it('displays link to privacy policy', () => {
        // ensure logged in and welcome modal is visible
        clearAllStorage();
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // verify link to privacy policy is visible
        cy.contains('Privacy Policy').should('be.visible');
        cy.contains('Privacy Policy').should('have.attr', 'href', 'https://www.chef.io/privacy-policy/');
        cy.contains('Privacy Policy').should('have.attr', 'target', '_blank');
      });

      it('can be closed with "Close" button', () => {
        // ensure logged in and welcome modal is visible
        clearAllStorage();
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // click button
        cy.get('[data-cy=close-welcome]').click();

        // verify welcome modal is not visible
        cy.get('[data-cy=welcome-title]').should('not.be.visible');
      });

      it('can be closed with "X" button', () => {
        // ensure logged in and welcome modal is visible
        clearAllStorage()
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // click button
        cy.get('[data-cy=close-x]').click();

        // verify welcome modal is not visible
        cy.get('[data-cy=welcome-title]').should('not.be.visible');
      });

      it('can be closed by clicking outside the modal container', () => {
        // ensure logged in and welcome modal is visible
        clearAllStorage()
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');

        // click outside modal
        cy.get('app-welcome-modal .modal-overlay').click({ force: true });

        // verify welcome modal is not visible
        cy.get('[data-cy=welcome-title]').should('not.be.visible');
      });

      it('can be reopened from profile menu', () => {
        // ensure logged in and welcome modal is not visible
        clearAllStorage();
        login();
        cy.get('[data-cy=welcome-title]').should('be.visible');
        closeModal();
        cy.get('[data-cy=welcome-title]').should('not.be.visible');

        // open profile menu and click welcome button
        cy.get('[data-cy=user-profile-button]').click();
        cy.get('[data-cy=welcome-modal-button]').click();

        // verify welcome modal is visible
        cy.get('[data-cy=welcome-title]').should('be.visible');
      });
    });
  });
}

function clearAllStorage() {
  cy.clearCookies();
  cy.clearLocalStorage();
}

function login() {
  cy.visit('/event-feed');

  cy.contains('Sign in as a local user').click({ force: true });

  cy.get('#login').type('admin');
  cy.get('#password').type('chefautomate');
  cy.get('[type=submit]').click();

  cy.url().should('include', '/event-feed');
  cy.get('h1.page-title').should('be.visible');
  cy.get('app-welcome-modal').should('exist');
  cy.contains('Local Administrator').should('exist');
}

function logout() {
  cy.get('[data-cy=user-profile-button]').click();
  cy.get('[data-cy=sign-out-button]').click();
}

function closeModal() {
  cy.get('[data-cy=close-welcome]').click();
}
