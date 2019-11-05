if (Cypress.env('TEST_BUILDER')) {
    describe('builder smoke', () => {
        it('can login, create a token and an origin', () => {
            cy.visit('/bldr/');
            cy.log('Logging in');
            cy.get('.button').contains('Sign In').click();
            cy.location('hash').should('include', '#/sign-in');
            cy.get('.button').contains('Sign In with Chef Automate').click();
            cy.location('pathname')
                .then((path: any) => path.startsWith('/dex/auth/local'))
                .then((local: any) => {
                    if (!local) {
                        return cy.get('a').contains('Sign in as a local user').click();
                    }
                });
            cy.get('#login').type('admin');
            cy.get('#password').type('chefautomate');
            cy.get('[type=submit]').click();
            cy.location('hash').should('include', '#/origins');
            cy.log('Creating token');
            cy.visit('/bldr/#/profile');
            cy.get('.generate').contains('Generate Token').click();
            cy.get('.copyable-component>span').first().then(el => {
                return cy.writeFile('token', el.text());
            });

            const origin = Math.random().toString(36).substring(7);

            cy.log(`Creating Origin "${origin}"`);
            cy.visit('/bldr/#/origins/create');
            cy.get('[type=text]').clear().type(origin);
            // we have to wait because there's no easy way to tell if the save button is
            // clickable. It has a disabled attribute when its not clickable, however its
            // value is "". Cypress assertions seem to think thats the same as not being
            // there.
            cy.get('button').contains('Save & Continue').wait(5000).click().then(() => {
                return cy.writeFile('origin', origin);
            });
            cy.location('hash').should('include', `#/origins/${origin}/packages`);
        });
    });
}
