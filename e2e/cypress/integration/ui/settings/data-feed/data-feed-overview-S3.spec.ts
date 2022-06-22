describe('chef datafeed', () => {


  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;
  const S3 = 'S3';
  const Storage = 'Storage';
  const S3Bucket = 'mybucket',
    S3Secret = 'S3Secret',
    S3Access = 'S3Access',
    S3Region = 'ap-south-1';

  before (() => {
    cy.adminLogin('/settings').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.get('app-welcome-modal').invoke('hide');
      cy.restoreStorage();
    });
    cy.restoreStorage();
  });


  afterEach(() => {
    cy.saveStorage();
  });

  describe ('chef data feed page for S3', () => {
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
            key: 'access_key',
            value: S3Access
          },
          {
            key: 'secret_access_key',
            value: S3Secret
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
          url: 'null',
          name: name,
          secret: secrectResp.body.id,
          services: S3,
          integration_types: Storage,
          meta_data: [
            {
              key: 'bucket',
              value: S3Bucket
            },
            {
              key: 'region',
              value: S3Region
            }
          ]
        }
        }).then((dataFeedResp) => {
          expect(dataFeedResp.status).to.equal(200);
          expect(dataFeedResp.body).to.property('id');
          cy.visit(`/settings/data-feeds/${dataFeedResp.body.id}`);
          cy.restoreStorage();
          cy.get('app-welcome-modal').invoke('hide');
        });
      });
    });

    it('test connection in data feed S3 failure', () => {
      cy.get('[data-cy=test-connection]').click();
      cy.get('app-notification.error').should('be.visible');
      cy.get('app-notification.error chef-icon').click();
    });

    it('check if clicking on save after changing name and url ', () => {
      cy.get('[data-cy=name-input]').type('-1');
      cy.get('[data-cy=bucket-input]').type('-1');
      cy.get('[data-cy=select-region-type]').click();
      cy.get('[data-cy=ap-northeast-1]').click();
      cy.get('[data-cy=save-connection]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains(name + '-1').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });
  });
});
