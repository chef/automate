describe('login the app', () => {
    const uuid = 'CiQ3N2NkODAzZi0xOWRiLTQxNDUtYmUzZS1lMDE2MDI3OWU4YzYSBWxvY2Fs';
    describe('save & restore the telemetry preference', () => {
        const warningText = 'For the changes to take effect, please logout and re-login again';
        before(() => {
            cy.visit('/');
            cy.contains('Sign in as a local user').click({ force: true }).then(() => {
                cy.get('#login').type('admin');
                cy.get('#password').type('chefautomate');
                cy.get('[type=submit]').click();
            });
        });
        it('confirm the warning text added in telemetry preference', () => {
            cy.get('[data-cy=telemetry-pref-checkbox]').should('exist').then(() => {
                cy.get('[data-cy=telemetry-pref-checkbox] .label-wrap')
                .contains(warningText).should('exist');
            });
        });

        it('enable and save the telemetry pref', function () {
            cy.get('[data-cy=welcome-title]').should('exist').then(() => {
                cy.get('[data-cy=close-x]').click().then(() => {
                    expect(localStorage.getItem(uuid + '-telemetry-enabled')).to.equal('true');
                });
            });
        });

        it('restore the enabled telemetry pref', function () {
            cy.get('[data-cy=user-profile-button]').click({ force: true }).then(() => {
                cy.get('[data-cy=welcome-modal-button]').click({ force: true }).then(() => {
                    cy.get('[data-cy=welcome-title]').should('exist').then(() => {
                        cy.get('[data-cy=telemetry-pref-checkbox]')
                        .invoke('attr', 'aria-checked').should('eq', 'true');
                        cy.get('[data-cy=close-x]').click().then(() => {
                            expect(localStorage.getItem(uuid + '-telemetry-enabled'))
                            .to.equal('true');
                        });
                    });
                });
            });
        });

        it('disable and save the telemetry pref', function () {
            cy.get('[data-cy=user-profile-button]').click({ force: true }).then(() => {
                cy.get('[data-cy=welcome-modal-button]').click({ force: true }).then(() => {
                    cy.get('[data-cy=welcome-title]').should('exist').then(() => {
                        // uncheck the telemetry checkbox
                        cy.get('[data-cy=telemetry-pref-checkbox]').click().then(() => {
                            cy.get('[data-cy=telemetry-pref-checkbox]')
                            .invoke('attr', 'aria-checked').should('eq', 'false');
                            cy.get('[data-cy=close-x]').click();
                            expect(localStorage.getItem(uuid + '-telemetry-enabled'))
                            .to.equal('false');
                        });
                    });
                });
            });
        });

        it('restore the disabled telemetry pref', function () {
            cy.get('[data-cy=user-profile-button]').click({ force: true }).then(() => {
                cy.get('[data-cy=welcome-modal-button]').click({ force: true }).then(() => {
                    cy.get('[data-cy=welcome-title]').should('exist').then(() => {
                        cy.get('[data-cy=telemetry-pref-checkbox]')
                        .invoke('attr', 'aria-checked').should('eq', 'false');
                        cy.get('[data-cy=close-x]').click().then(() => {
                            expect(localStorage.getItem(uuid + '-telemetry-enabled'))
                            .to.equal('false');
                        });
                    });
                });
            });
        });

    });

    describe('start the telemetry services after login', () => {
        before(() => {
            cy.visit('/');
            cy.contains('Sign in as a local user').click({ force: true }).then(() => {
                cy.get('#login').type('admin');
                cy.get('#password').type('chefautomate');
                cy.get('[type=submit]').click();
            });
        });

        it('run the telemetry services if pref is enabled ', function () {
            cy.get('[data-cy=welcome-title]').should('exist').then(() => {
                cy.get('[data-cy=close-x]').click().then(() => {
                    expect(localStorage.getItem(uuid + '-telemetry-enabled')).to.equal('true');
                    cy.server();
                    cy.route({
                        method: 'GET',
                        url: 'https://telemetry-acceptance.chef.io/segment/api_keys'
                    }).as('telemetryWriteKey');
                    cy.route({
                        method: 'POST',
                        url: 'https://api.segment.io/v1/p'
                    }).as('pageTracker');

                    // reload the page and see telemetry services are running if pref is enabled
                    cy.reload().then(() => {
                        cy.wait('@telemetryWriteKey').its('status').should('be', 200);
                        cy.wait('@pageTracker').its('status').should('be', 200);
                    });
                });
            });
        });
    });

});
