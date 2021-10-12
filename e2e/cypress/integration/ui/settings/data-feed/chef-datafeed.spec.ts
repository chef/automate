describe('chef datafeed', () => {

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
      cy.contains('Data Feeds').click();
      cy.get('[data-cy=create-data-feed]').should('exist');
    });
  });
});
