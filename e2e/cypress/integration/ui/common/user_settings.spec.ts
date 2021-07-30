describe('login the app', () => {
    describe('load and update user preference', () => {
        let adminIdToken = '';
        let timeformat = 'ddd, DD MMM YYYY';
        let username = '';
        let connector = '';
        before(() => {
            cy.adminLogin('/').then(() => {
              const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
              adminIdToken = admin.id_token;
              username = admin.username;
              connector = admin.connector;
              cy.request({
                auth: { bearer: adminIdToken },
                failOnStatusCode: false,
                method: 'GET',
                url: 'api/v0/user-settings/' + username + '/' + connector
              }).then((resp) => {
                if (resp.status === 200 && resp.statusText === 'OK') {
                  timeformat = resp.body.settings.date_format.value;
                  return;
                }
              });
            });
            cy.restoreStorage();
        });

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
                    cy.get('[data-cy=timeformat-dropdown] .selected-value .option-content')
                    .then(($element) => {
                        cy.get('[data-cy=timeformat-dropdown] .selected-value .option-content')
                        .contains(timeformat);
                    });

                });
            });
        });

        it('change and save timeformat', function () {
            const timeformatList = ['YYYY-M-D', 'YYYY-MM-DD'];
            let selectTimeformat = '';
            for (let i = 0; i < timeformatList.length; i++) {
                if (timeformat.indexOf(timeformatList[i]) === -1) {
                    selectTimeformat = timeformatList[i];
                    break;
                }
            }
            cy.request({
                auth: { bearer: adminIdToken },
                failOnStatusCode: false,
                method: 'PUT',
                url: 'api/v0/user-settings/' + username + '/' + connector,
                body: {
                    user: {
                        name: username,
                        connector: connector
                    },
                    settings: {
                      date_format: {
                        value: selectTimeformat
                      }
                    }
                }
              }).then((resp) => {
                if (resp.status === 200 && resp.statusText === 'OK') {
                  expect(resp.status).to.equal(200);
                  return;
                }
            });
        });

    });

});
