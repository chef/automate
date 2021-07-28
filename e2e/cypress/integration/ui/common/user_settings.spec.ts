describe('login the app', () => {
    describe('load and update user preference', () => {

        it('open profile menu if exist', function () {
            cy.get('[data-cy=user-profile-button]').should('exist').then(() => {
                cy.get('[data-cy=user-profile-button]').click().then(() => {
                    cy.get('.dropdown-list-item .profile').should('exist');
                });
            });
        });

        it('timeformat added in user detail page', function () {
            cy.get('.dropdown-list-item .profile').click().then(() => {
                cy.get('app-user-details').should('exist').then(() =>  {
                    cy.get('[data-cy=timeformat-dropdown]').should('exist');
                });
            });
        });

        it('change and save timeformat value', function () {
            cy.get('[data-cy=timeformat-dropdown]').click().then(() => {
                cy.get('[value=YYYY-M-D]').click().then(() => {
                    cy.get('[data-cy=timeformat-dropdown]').should('have.value', 'YYYY-M-D')
                    .then(() => {
                        cy.server();
                        cy.route({
                            method: 'PUT',
                            url: 'https://a2-dev.test/api/v0/user-settings/admin/local'
                        }).as('updateUserPreference');
                        cy.get('[data-cy=user-details-submit-button]').click().then(() => {
                            cy.wait('@updateUserPreference').its('status').should('be', 200);
                        });
                    });
                });
            });
        });

    });

});
