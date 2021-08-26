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
      cy.get('body').type('feat');
      cy.get('.title').contains('Chef Automate Data Feed').parent().parent()
        .find('.onoffswitch').click();
      cy.get('chef-button').contains('Close').click();
      cy.reload();
      cy.contains('know').click();
      cy.contains('Data Feeds').click();
      cy.get('[data-cy=create-data-feed]').should('exist');
    });
  });
});
