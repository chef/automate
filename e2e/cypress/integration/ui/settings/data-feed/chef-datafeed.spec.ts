describe('chef datafeed', () => {
  const name = 'cytest',
  url = 'http://test.com',
  username = 'admin',
  password = 'password',
  tokenType = 'TestType',
  token = 'behwveh3238238=';
  const reusableDate = Date.now();

  before(() => {
    cy.adminLogin('/settings/notifications').then(() => {
        const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
    });
      cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  describe ('chef data feed page', () => {

    it('show data feed', () => {
      cy.get('body').type('feat');
      cy.get('.title').contains('Chef Automate Data Feed').parent().parent()
        .find('.onoffswitch').click();
      cy.get('chef-button').contains('Close').click();
      cy.reload();
      cy.contains('know').click();
      cy.contains('Data Feeds').click();
      cy.get('[data-cy=create-data-feed]').should('exist');
    });

    it('create data feed custom username/password', () => {
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

    it('create data feed custom token', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + date);
      cy.get('[data-cy=add-url]').type(url);
      cy.contains('Edit').click();
      cy.get('[data-cy=add-token-type]').type(tokenType);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info chef-icon').click();
      cy.contains('Data Feeds').click();
      cy.get('chef-table chef-tbody chef-td').contains('cytest' + date).should('exist');
    });

  });
});
