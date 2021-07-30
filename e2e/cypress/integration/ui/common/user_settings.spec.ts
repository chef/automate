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

    });

});
