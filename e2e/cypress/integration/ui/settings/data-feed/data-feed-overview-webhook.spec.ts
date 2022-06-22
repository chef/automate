describe('chef datafeed', () => {

  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;
  const url = 'https://localhost/api/x_chef_automate/asset';
  const ServiceNow = 'Service Now';
  const Webhook = 'Webhook';

  before (() => {
    cy.adminLogin('/settings').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.get('app-welcome-modal').invoke('hide');
      cy.restoreStorage();
    });
    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });


  afterEach(() => {
    cy.saveStorage();
  });

  describe ('chef data feed page for Webhook', () => {
    before(() => {
      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/api/v0/secrets',
        body: {
          name: 'anyname',
          type: 'data_feed',
          data: [
            {
              key: 'headers',
              value: '{\"Authorization\":\"Basic service-now-token\"}'
            },
            {
              key: 'auth_type',
              value: ''
            }
          ]
        }
      }).then((secrectResp) => {
        expect(secrectResp.status).to.equal(200);
        expect(secrectResp.body).to.property('id');

        cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/api/v0/datafeed/destination',
        body: {
          url: url,
          name: name,
          secret: secrectResp.body.id,
          services: ServiceNow,
          integration_types: Webhook
        }
        }).then((dataFeedResp) => {
          expect(dataFeedResp.status).to.equal(200);
          expect(dataFeedResp.body).to.property('id');
          cy.visit(`/settings/data-feeds/${dataFeedResp.body.id}`);
          cy.get('app-welcome-modal').invoke('hide');
          cy.restoreStorage();
        });
      });
    });

    it('check if clicking on disable', () => {
      cy.get('[data-cy=disable-btn]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains('Disabled').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });

    it('check if clicking on enable', () => {
      cy.get('[data-cy=enable-btn]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains('Enabled').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });

    it('check if clicking on save after changing name and url ', () => {
      cy.get('[data-cy=name-input]').type('-1');
      cy.get('[data-cy=url-input]').type('/v1');
      cy.get('[data-cy=save-connection]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains(name + '-1').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });

    it('check if clicking on test connection', () => {
      cy.get('[data-cy=name-input]').type('-1');
      cy.get('[data-cy=url-input]').type('/v1');
      cy.get('[data-cy=save-connection]').click();
    });

    it('check GlobalDataFeedConfig details', () => {
      cy.get('[data-cy=feed-interval]').should('not.be.empty');
      cy.get('[data-cy=node-batch-size]').should('not.be.empty');
      cy.get('[data-cy=updated-nodes-only]').should('not.be.empty');
      cy.get('[data-cy=disable-cidr-filter]').should('not.be.empty');
      cy.get('[data-cy=cidr_filter]').find('div').eq(2).should('not.be.empty');
      cy.get('[data-cy=accepted-status-codes]').find('div').eq(2).should('not.be.empty');
    });

    it('check if clicking on Delete', () => {
      cy.get('[data-cy=delete-btn]').click();
      cy.get('app-delete-object-modal').find('button').contains('Delete Data Feed')
        .click({force: true});
      cy.get('chef-tbody').contains(name).should('not.exist');
    });
  });

});
