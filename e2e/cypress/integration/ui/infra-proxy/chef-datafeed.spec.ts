describe('chef datafeed', () => {
  const now  = Cypress.moment().format('MMDDYYhhmmss');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const generatedServerID = serverName.split(' ').join('-');
  const customServerID = `${cypressPrefix}-custom-id-${now}`;
  const serverFQDN = 'chef-server-1617089723092818000.com';
  const serverIP = '176.119.50.159';

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
        //to get the feature flag   
      cy.get('body').type('feat');
      cy.get('.title').contains('Chef Automate Data Feed').parent().parent()
        .find('.onoffswitch').click();
      cy.get('chef-button').contains('Close').click();
      cy.reload();
      cy.contains('know').click();
      cy.contains('Data Feeds').click();
      cy.get('[data-cy=new-integration]').should('exist');
    });
  });
});
