describe('login the app', () => {
  let adminIdToken = '';
  let username = '';
  let connector = '';
  describe('load and update user preference', () => {
    let timeformat = 'ddd, DD MMM YYYY';
    const count = 0;
    const attrSelector = '';
    const timeformatList = ['YYYY-M-D', 'YYYY-MM-DD'];
    let selectTimeformat = '';
    let telemetryEnabled = false;
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
        cy.request({
          auth: { bearer: adminIdToken },
          failOnStatusCode: false,
          method: 'GET',
          url: 'api/v0/telemetry/config'
        }).then((resp) => {
          if (resp.status === 200 && resp.statusText === 'OK') {
            telemetryEnabled = resp.body.telemetry_enabled;
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

    it('timeformat value presenent on user detail page', function () {
      cy.get('app-user-details').should('exist').then(() =>  {
          cy.get('[data-cy=timeformat-dropdown]').should('exist');
          cy.get('[data-cy=timeformat-dropdown]').click()
            .get('mat-option').contains(timeformat).click();
      });
    });

    it('telemetry-checkbox updated in user details form', function () {
      cy.get('app-user-details').should('exist').then(() =>  {
          cy.get('[data-cy=telemetry-checkbox]').should('exist');
          cy.get('[data-cy=telemetry-checkbox] chef-icon').contains('check');
          expect(telemetryEnabled).to.equal(true);
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

        cy.get('mat-select').first().click(); // opens the drop down

        // simulate click event on the drop down item (mat-option)
        cy.get('.mat-option-text').contains(selectTimeformat).then(option => {
          option[0].click();  // this is jquery click() not cypress click()
          cy.get('[data-cy=timeformat-dropdown]')
            .contains(selectTimeformat)
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

    it('change and save telemetry-checkbox', function () {
      cy.get('[data-cy=telemetry-checkbox]').then(() => {
        cy.get('[data-cy=telemetry-checkbox]').click().then(() => {
          cy.get('[data-cy=user-details-submit-button]').click();
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
