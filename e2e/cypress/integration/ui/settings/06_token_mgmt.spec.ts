describe('token management', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'aaa-testing-tokens'; // ensure they load somewhere at the top of the list
  const tokenName1 = `${cypressPrefix} token 1 ${now}`;
  const tokenName2 = `${cypressPrefix} token 2 ${now}`;
  const tokenID1 = `${cypressPrefix}-id-1-${now}`;
  const tokenID2 = `${cypressPrefix}-id-2-${now}`;
  const tokensTable = 'app-api-tokens chef-table';

  before(() => {
    cy.adminLogin('/settings/tokens').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/api/v0/auth/tokens',
        body: {
          id: tokenID1,
          description: tokenName1,
          active: true
        }
      });
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/api/v0/auth/tokens',
        body: {
          id: tokenID2,
          description: tokenName2,
          active: false
        }
      });

      // load new tokens
      cy.reload(true);
      cy.get('app-welcome-modal').invoke('hide');
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    // can still use v2beta APIs while on v1
    cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix, ['tokens']);
  });

  it('displays heading and table', () => {
    cy.get('chef-heading').contains('API Tokens');
    cy.get(tokensTable).should('exist');
  });

  it('displays the returned tokens info', () => {
    cy.get(`${tokensTable} chef-td`).contains(tokenName1);
    cy.get(`${tokensTable} chef-td`).contains(tokenName2);

    cy.get('chef-table chef-td').contains(tokenName1).parent()
      .get('chef-td').contains('Active');
    cy.get('chef-table chef-td').contains(tokenName2).parent()
      .get('chef-td').contains('Inactive');
  });

  it('control menu has all options', () => {
    // ['Copy Token', 'Toggle Status', 'Delete Token'].forEach((item, index) => {

    // cy.get('chef-table chef-td').contains(tokenName1).parent()
    //   .find('chef-control-menu').as('controlMenu');
    // // we throw in a should so cypress waits until introspection allows menu to be shown
    // cy.get('@controlMenu').should('be.visible')
    //   .click();
    // assert: expect to see item in control menu
    // });
  });

  it('can copy token', () => {
    // cy.get('chef-table chef-td').contains(tokenName1).parent()
    //   .find('chef-control-menu').as('controlMenu');
    // // we throw in a should so cypress waits until introspection allows menu to be shown
    // cy.get('@controlMenu').should('be.visible')
    //   .click();
    // cy.get('@controlMenu').find('[data-cy=delete]').click({ force: true });
  });

  it('can delete token', () => {
    // cy.get('app-user-table chef-td').contains(tokenName1).parent()
    //   .find('chef-control-menu').as('controlMenu');
    // // we throw in a should so cypress waits until introspection allows menu to be shown
    // cy.get('@controlMenu').should('be.visible')
    //   .click();
    // cy.get('@controlMenu').find('[data-cy=delete]').click({ force: true });
  });

  // TODO
  it('can create a new token', () => {
  });
});

describe('token details', () => {
  // cy.visit(`/settings/tokens/${tokenId2}`);

  it('renders all token details', () => {
    // displays the id in the header
    // displays the date in the header
    // displays the status in the header
    // displays the ID in the details input
  });
});
