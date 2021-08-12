describe('login the app', () => {
  let adminIdToken = '';
  let username = '';
  let connector = '';
  describe('load and update user preference', () => {
    let timeformat = 'ddd, DD MMM YYYY';
    let count = 0;
    let attrSelector = '';
    const timeformatList = ['YYYY-M-D', 'YYYY-MM-DD'];
    let selectTimeformat = '';
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
        cy.visit('/user-details/admin').then(() => {
          cy.get('[data-cy=welcome-title]').should('exist').then(() => {
            cy.get('[data-cy=close-x]').click();
          });
        });
      });
      cy.restoreStorage();
    });

    it('timeformat updated in user detail page', function () {
      cy.get('app-user-details').should('exist').then(() =>  {
          cy.get('[data-cy=timeformat-dropdown]').should('exist');
          cy.get('[data-cy=timeformat-dropdown] .selected-value .option-content')
          .then(($element) => {
              cy.get('[data-cy=timeformat-dropdown] .selected-value .option-content')
              .contains(timeformat);
          });

      });
    });

    it('change and save timeformat value', function () {
      cy.get('[data-cy=timeformat-dropdown]').then((dropdown) => {
        selectTimeformat = '';
        for (let i = 0; i < timeformatList.length; i++) {
            if (dropdown[0].innerText.indexOf(timeformatList[i]) === -1) {
                selectTimeformat = timeformatList[i];
                break;
            }
        }
        attrSelector = '[value=' + selectTimeformat + ']';
        cy.get('[data-cy=timeformat-dropdown]').click().then(() => {
          cy.get(attrSelector).click({force: true}).then(() => {
            count = 0;
            while (dropdown[0].innerText.indexOf(selectTimeformat) !== 0 && count < 10) {
                cy.get(attrSelector).click({force: true});
                count++;
            }
            cy.get('[data-cy=timeformat-dropdown]')
              .should('have.value', selectTimeformat)
              .then(() => {
                cy.server();
                cy.route({
                    method: 'PUT',
                    url: 'api/v0/user-settings/' + username + '/' + connector
                }).as('updateUserPreference');
                cy.get('[data-cy=user-details-submit-button]')
                  .click({force: true}).then(() => {
                    cy.wait('@updateUserPreference')
                      .its('status')
                      .should('be', 200);
                });
            });
          });
        });
      });
    });

  });

  describe('api testing', () => {
    describe('test get api - user preference', () => {
      let getApiStatus = 'initialize';
      before(() => {
        cy.request({
          auth: { bearer: adminIdToken },
          failOnStatusCode: false,
          method: 'GET',
          url: 'api/v0/user-settings/' + username + '/' + connector
        }).then((resp) => {
          if (resp.status === 200 && resp.statusText === 'OK') {
            getApiStatus = 'success';
          } else {
            getApiStatus = 'error';
          }
        });
      });

      it('response status 200', function () {
        expect(getApiStatus).to.equal('success');
      });

    });

    describe('test update api - user preference', () => {
      let updateApiStatus = 'initialize';
      before(() => {
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
                  value: 'YYYY-MM-DD'
                }
              }
          }
        }).then((resp) => {
          if (resp.status === 200 && resp.statusText === 'OK') {
            updateApiStatus = 'success';
          } else {
            updateApiStatus = 'error';
          }
        });
      });

      it('response status 200', function () {
        expect(updateApiStatus).to.equal('success');
      });

    });
  });

});
