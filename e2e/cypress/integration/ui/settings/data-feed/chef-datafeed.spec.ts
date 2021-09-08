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

    it('create data feed error', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=Custom]').click();
      cy.get('[data-cy=add-name]').type(name + reusableDate);
      cy.get('[data-cy=add-url]').type(url);
      cy.get('[data-cy=add-token]').type(token);
      cy.get('[data-cy=add-button]').click();
      cy.get('app-data-feed-create').scrollTo('top');
      cy.get('app-notification.error').should('be.visible');
      cy.get('app-notification.error chef-icon').click();
      cy.get('[data-cy=close-feed-button]').click();
    });
  });
});
