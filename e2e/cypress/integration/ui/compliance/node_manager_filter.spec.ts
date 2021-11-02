describe('Scan job', () => {

  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;

  before(() => {
      cy.adminLogin('/').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminIdToken = admin.id_token;
          cy.get('app-welcome-modal').invoke('hide');
          cy.restoreStorage();
      });
      cy.visit('/jobs/add');
      cy.get('app-welcome-modal').invoke('hide');
    });

  beforeEach(() => {
      cy.restoreStorage();
    });

  afterEach(() => {
      cy.saveStorage();
  });

  describe ('add node in scan jobs', () => {
   before(() => {
      cy.request({
      auth: { bearer: adminIdToken },
      method: 'POST',
      url: '/api/v0/secrets/search',
      body: {}
      }).then((secrectSearchResp) => {
            cy.request({
              auth: { bearer: adminIdToken },
              method: 'POST',
              url: '/api/v0/nodemanagers',
              body: {
                'name':'AWStest',
                'type':'aws-ec2',
                'credential_data':[{'key':'AWS_ACCESS_KEY_ID','value':Cypress.env('AWS_ACCESS_KEY_ID')},
                {'key':'AWS_SECRET_ACCESS_KEY',
                'value':Cypress.env('AWS_SECRET_ACCESS_KEY')
              }]
            }
            }).then((secrectResp) => {
              expect(secrectResp.status).to.equal(200);
            });
      });

      cy.request({
      auth: { bearer: adminIdToken },
      method: 'POST',
      url: '/api/v0/secrets/search',
      body: {}
      }).then((secrectSearchResp) => {
            cy.request({
              auth: { bearer: adminIdToken },
              method: 'POST',
              url: '/api/v0/nodemanagers',
              body: {
                'name': 'AzureFinal',
                'type': 'azure-api',
                "credential_data": [
    {
      'key': 'AZURE_CLIENT_ID',
      'value': 'Cypress.env(SECRET_AZURE_CLIENT_ID)'
    },
    {
      'key': 'AZURE_CLIENT_SECRET',
      'value': 'Cypress.env(SECRET_AZURE_CLIENT_SECRET)'
    },
    {
      'key': 'AZURE_TENANT_ID',
      "value": 'Cypress.env(SECRET_AZURE_TENANT_ID)'
    }
  ],
   "instance_credentials": []

            }
            }).then((secrectResp) => {
              expect(secrectResp.status).to.equal(200);
            });
      });
    });


    it('check filter for automate', () => {
      cy.get('[data-cy=automate_click]').click();
      cy.get('[data-cy=automate-0]').should('be.visible');
    });

    it('check filter for aws', () => {
      cy.get('[data-cy=automate_click]').click();
      cy.get('[data-cy=aws_click]').click();
      cy.get('[data-cy=aws-ec2-0]').should('be.visible');
    });

    it('check filter for azure', () => {
      cy.get('[data-cy=aws_click]').click();
      cy.get('[data-cy=azure_click]').click();
      cy.get('[data-cy=azure-0]').should('be.visible');
    });

    it('check filter for gcp', () => {
      cy.get('[data-cy=gcp_click]').click();
      cy.get('[data-cy=gcp-0]').should('not.be.visible');
    });



  });

});
