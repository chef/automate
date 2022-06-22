describe('chef datafeed', () => {

  const name = 'cytest',
    url = 'http://test.com',
    username = 'admin',
    password = 'password',
    tokenType = 'TestType',
    token = 'behwveh3238238=';


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
    let delete_name: string;
    it('check if clicking on new integration button opens up the slider', () => {
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=interation-menu]').should('be.visible');
      cy.get('[data-cy=close-feed-button]').click();
    });

    it('create data feed service now', () => {
      const date = Date.now();
      cy.get('[data-cy=create-data-feed]').click();
      cy.get('[data-cy=ServiceNow]').click();
      delete_name = name + date;
      cy.get('[data-cy=add-name]').type(delete_name);
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

    it('check if clicking on disable', () => {
      cy.get('chef-tr').contains(delete_name).parent().parent().
      find('.mat-select-trigger').click();
      cy.get('[data-cy=disable-btn]').click();
      cy.get('app-notification.info').should('be.visible');
      cy.get('app-notification.info p').contains('Disabled').should('exist');
      cy.get('app-notification.info chef-icon').click();
    });

    it('check if clicking on enable', () => {
        cy.get('chef-tr').contains(delete_name).parent().parent()
        .find('.mat-select-trigger').click();
        cy.get('[data-cy=enable-btn]').click();
        cy.get('app-notification.info').should('be.visible');
        cy.get('app-notification.info p').contains('Enabled').should('exist');
        cy.get('app-notification.info chef-icon').click();
    });


    it('check if clicking on Cancel works', () => {
      cy.get('chef-tr').contains(delete_name).parent().parent()
      .find('.mat-select-trigger').click();
      cy.get('[data-cy=remove-data-feed]').click();
      cy.get('app-data-feed-table chef-modal').find('chef-button').contains('Cancel').click();
    });

    it('check if clicking on Delete works', () => {
      cy.get('chef-tr').contains(delete_name).parent().parent()
      .find('.mat-select-trigger').click();
      cy.get('[data-cy=remove-data-feed]').click();
      cy.get('app-data-feed-table chef-modal').find('chef-button').contains('Delete').click();
      cy.get('chef-tr').contains(delete_name).should('not.exist');
    });

  });
});
