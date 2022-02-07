describe('login the app', () => {
  let adminIdToken = '';
  let username = '';
  let connector = '';
  let isDataloaded = false;
  let isLargeReportingEnabled = false;
  describe('large compliance report', () => {
    before(() => {
      cy.adminLogin('/').then(() => {
        const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
        adminIdToken = admin.id_token;
        username = admin.username;
        connector = admin.connector;
        cy.visit('/compliance/reports/nodes').then(() => {
          cy.get('[data-cy=welcome-title]').should('exist').then(() => {
              cy.get('[data-cy=close-x]').click();
          });
        });
      });
      cy.restoreStorage();
    });
    describe('api testing', () => {

      it('is data exist for download', function() {
        cy.get('[data-cy=nodes-tab]').then(($anchor) => {
          const dataLabel = $anchor.text().trim();
          if (dataLabel.indexOf('0 Nodes') === 0 || dataLabel.indexOf('Nodes') === 0) {
            expect(isDataloaded).to.equal(false);
          } else {
            isDataloaded = true;
          }
        });
      });

      it('is large reporting enabled', function() {
        cy.get('[data-cy=download-dropdown]').then($dropdown => {
          // is LCR enabled
          if ($dropdown.find('[data-cy=open-report]').length > 0) {
            isLargeReportingEnabled = true;
          } else {
            expect(isLargeReportingEnabled).to.equal(false);
          }
        });
      });

      describe('test acknowledgement api', () => {
        let apiStatus = 'initialize';
        before(() => {
          cy.request({
            auth: { bearer: adminIdToken },
            failOnStatusCode: false,
            method: 'POST',
            url: '/api/v0/compliance/reporting/reportmanager/export',
            body: {
                'type': 'csv',
                'filters': []
            }
          }).then((resp) => {
            if (resp.status === 200 && resp.statusText === 'OK') {
                apiStatus = 'success';
            } else {
                apiStatus = 'error';
            }
          });
        });

        it('response status 200', function () {
          if (isDataloaded && isLargeReportingEnabled) {
            expect(apiStatus).to.equal('success');
          }
        });
      });

      describe('test report status list api', () => {
        let apiStatus = 'initialize';
        before(() => {
          cy.request({
            auth: { bearer: adminIdToken },
            failOnStatusCode: false,
            method: 'GET',
            url: '/api/v0/reportmanager/requests'
          }).then((resp) => {
            if (resp.status === 200 && resp.statusText === 'OK') {
                apiStatus = 'success';
            } else {
                apiStatus = 'error';
            }
          });
        });

        it('response status 200', function () {
          if (isLargeReportingEnabled) {
            expect(apiStatus).to.equal('success');
          }
        });
      });
    });

    describe('download the report', () => {

      it('acknowledge the report', function() {
        if (isDataloaded && isLargeReportingEnabled) {
          cy.server();
          cy.route({
              method: 'POST',
              url: '/api/v0/compliance/reporting/reportmanager/export'
          }).as('acknowledgement');

          cy.get('[data-cy=download-dropdown]').click().then(() => {
            cy.get('[data-cy=download-csv]').click().then(() => {
              cy.wait('@acknowledgement')
              .its('status')
              .should('be', 200);
            });
          });
        }
      });

      it('check the status of report', function() {
        if (isDataloaded && isLargeReportingEnabled) {
          cy.server();
          cy.route({
              method: 'GET',
              url: '/api/v0/reportmanager/requests'
          }).as('reportStatus');

          cy.get('[data-cy=download-dropdown]').click().then(() => {
            cy.get('[data-cy=open-report]').click().then(() => {
              cy.wait('@reportStatus')
              .its('status')
              .should('be', 200);

              cy.get('[data-cy=download-report-list]').then(($table) => {
                if ($table.find('.csv').length > 0) {
                  cy.get('.csv').then(($csv) => {
                    expect($csv.find('.success-status').length).greaterThan(0);
                    cy.get('[data-cy=close-report-panel]').click();
                  });
                }
              });

            });
          });
        }

      });

      it('link to download the report', function() {
        if (isDataloaded && isLargeReportingEnabled) {
          cy.get('[data-cy=download-dropdown]').click().then(() => {
            cy.get('[data-cy=open-report]').click().then(() => {

              cy.get('[data-cy=download-report-list]').then(($table) => {
                if ($table.find('.csv').length > 0) {
                  cy.get('.csv').then(($csv) => {
                    if ($csv.find('.success-status').length > 0) {
                      cy.server();
                      cy.route({
                        method: 'GET',
                        url: '/api/v0/reportmanager/export/**'
                      }).as('downloadReport');

                      cy.get('.success-status .download-link').first().click().then(() => {
                        cy.wait('@downloadReport')
                        .its('status')
                        .should('be', 200);
                      });
                    }
                  });
                }
              });

            });
          });
        }
      });

    });
  });
});
