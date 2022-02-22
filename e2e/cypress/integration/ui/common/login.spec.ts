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
            cy.contains('Local Administrator'); // current user name
            // cy.screenshot()
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

      it('can reopen welcome modal from profile menu', () => {
        cy.get('[data-cy=user-profile-button]').click().then(() => {
          cy.get('[data-cy=welcome-modal-button]').click().then(() => {
            cy.get('[data-cy=welcome-title]').should('exist');
          });
        });
      });

      it('can close welcome modal with "X" button', () => {
        cy.get('[data-cy=close-x]').click().then(() => {
          cy.get('[data-cy=welcome-title]').should('not.exist');
        });
      });

      it('chef-automate-user object is created in localStorage', () => {
          cy.adminLogin('/').then(() => {
          const user = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          cy.wrap(user).should('be.a', 'object');
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
  });
}
