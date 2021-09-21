describe('chef datafeed', () => {

  const token = 'behwveh3238238=';
  const DestinationID = 'chef-server-dev-test';
  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;
  const url = 'https://localhost/api/x_chef_automate/asset';
  const ServiceNow = 'Service Now';
  const Minio = 'Minio';
  const S3 = 'S3';
  const Webhook = 'Webhook';
  const Storage = 'Storage';
  const minioBucket = 'mybucket',
  minioSecret = 'minioadmin',
  minioAccess = 'minioadmin',
  minioUrl = 'https://71a4-103-161-57-72.ngrok.io';

const S3Bucket = 'mybucket',
  S3Secret = 'S3Secret',
  S3Access = 'S3Access',
  S3Region = 'ap-south-1';

before (() => {
  cy.adminLogin('/').then(() => {
    const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
    adminIdToken = admin.id_token;
    cy.get('app-welcome-modal').invoke('hide');
    cy.restoreStorage();
    cy.get('body').type('feat');
    cy.get('.title').contains('Chef Automate Data Feed').parent().parent()
    .find('.onoffswitch').click();
    cy.get('chef-button').contains('Close').click();
    cy.reload();
    cy.contains('know').click();
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
        cy.get('app-notification.error').should('be.visible');
        cy.get('app-notification.error chef-icon').click();
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
