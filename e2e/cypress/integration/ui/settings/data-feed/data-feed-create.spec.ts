describe('chef datafeed', () => {

  const name = 'cytest',
    url = 'http://test.com',
    username = 'admin',
    password = 'password',
    tokenType = 'TestType',
    token = 'behwveh3238238=',
    endpoint = 'https://test.com',
    bucketName = 'bucket',
    accessKey = 'access_key',
    secretKey = 'secret_key';

  const minioBucket = 'mybucket',
    minioSecret = 'minioadmin',
    minioAccess = 'minioadmin',
    minioUrl = 'http://127.0.0.1:9000';


  before(() => {
    cy.adminLogin('/settings').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cy.get('app-welcome-modal').invoke('hide');
      cy.restoreStorage();
      cy.contains('Data Feeds').click();
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  describe ('chef data feed page', () => {
    const reusableDate = Date.now();

    it('check if clicking on new integration button opens up the slider', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=interation-menu]').should('be.visible');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('check if clicking on a interation opens the form', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ServiceNow]').click();
      cy.get('[data-cy=data-feed-create-form]').should('be.visible');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed service now', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ServiceNow]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('create data feed splunk', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + reusableDate).should('exist');
    });

    it('create data feed ELK', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ELK]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('create data feed error splunk', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('.data-feed-slider app-notification.error').should('be.visible');
      cy.get('.data-feed-slider app-notification.error chef-icon').click();
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed error ELK', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ELK]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('.data-feed-slider app-notification.error').should('be.visible');
      cy.get('.data-feed-slider app-notification.error chef-icon').click();
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed with changed token type Splunk', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=toggle-type]').click();
      cy.get('[data-cy=add-token-type]').clear();
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('create data feed with changed token type ELK', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ELK]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-access-token]').click();
      cy.get('[data-cy=toggle-type]').click();
      cy.get('[data-cy=add-token-type]').clear();
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('test error in data feed Splunk', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed Custom with token', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('test connection in data feed Custom', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed Custom with username', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('create data feed error custom', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('.data-feed-slider app-notification.error').should('be.visible');
      cy.get('.data-feed-slider app-notification.error chef-icon').click();
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('test error in data feed ELK for Token', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ELK]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-access-token]').click();
      cy.get('[data-cy=toggle-type]').click();
      cy.get('[data-cy=add-token-type]').clear();
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('test error in data feed ELK for username and password', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ELK]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed minio', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(endpoint);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('test error in data feed minio failure', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(endpoint);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('test connection in data feed minio success', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(minioUrl);
      cy.get('[data-cy=add-bucket-name]').type(minioBucket);
      cy.get('[data-cy=add-access-key]').type(minioAccess);
      cy.get('[data-cy=add-secret-key]').type(minioSecret);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed for S3', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy="S3"]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=add-button]').click();
      cy.get('[data-cy=close-feed-button]').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + reusableDate).should('exist');
    });

    it('error in creating data feed for S3', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy="S3"]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('test faliure in data feed S3', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('app-data-feed-create').scrollTo('bottom');
      cy.get('[data-cy="S3"]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });
  });
  describe ('chef data feed page', () => {
    const reusableDate = Date.now();

    it('check if clicking on new integration button opens up the slider', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=interation-menu]').should('be.visible');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('check if clicking on a interation opens the form', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ServiceNow]').click();
      cy.get('[data-cy=data-feed-create-form]').should('be.visible');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed service now', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ServiceNow]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=select-auth-type]').click();
      cy.get('[data-cy=select-username-password]').click();
      cy.get('[data-cy=add-username]').type(username);
      cy.get('[data-cy=add-password]').type(password);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('create data feed splunk', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('[data-cy=close-feed-button]').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + reusableDate).should('exist');
    });

    it('create data feed error', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed with changed token type', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=toggle-type]').click();
      cy.get('[data-cy=add-token-type]').clear();
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('test error in data feed', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Splunk]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed minio', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(endpoint);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

    it('test error in data feed minio failure', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(endpoint);
      cy.get('[data-cy=add-bucket-name]').type(bucketName);
      cy.get('[data-cy=add-access-key]').type(accessKey);
      cy.get('[data-cy=add-secret-key]').type(secretKey);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('test connection in data feed minio success', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Minio]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-endpoint]').type(minioUrl);
      cy.get('[data-cy=add-bucket-name]').type(minioBucket);
      cy.get('[data-cy=add-access-key]').type(minioAccess);
      cy.get('[data-cy=add-secret-key]').type(minioSecret);
      cy.get('[data-cy=test-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('[data-cy=close-feed-button]').click();
    });
  });
});
