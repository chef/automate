describe('chef datafeed', () => {

  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;
  const Minio = 'Minio';

  const Storage = 'Storage';
  const minioBucket = 'mybucket',
    minioSecret = 'minioadmin',
    minioAccess = 'minioadmin',
    minioUrl = 'http://127.0.0.1:9000';

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

  describe ('chef data feed page for Minio', () => {
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
            value: minioAccess
          },
          {
            key: 'secret_access_key',
            value: minioSecret
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
          url: minioUrl,
          name: name,
          secret: secrectResp.body.id,
          services: Minio,
          integration_types: Storage,
          meta_data: [
          {
            key: 'bucket',
            value: minioBucket
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

    it('test connection in data feed minio success', () => {
      cy.get('[data-cy=test-connection]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
    });

    it('check if clicking on save after changing name and url ', () => {
      cy.get('[data-cy=name-input]').type('-1');
      cy.get('[data-cy=url-input]').type('/v1');
      cy.get('[data-cy=bucket-input]').type('-1');
      cy.get('[data-cy=save-connection]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains(name + '-1').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });

    it('test connection in data feed minio failure', () => {
      cy.get('[data-cy=test-connection]').click();
      cy.get('app-notification.error').should('be.visible');
      cy.get('app-notification.error chef-icon').click();
    });
  });
});
