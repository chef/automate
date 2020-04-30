import { itFlaky } from '../../../support/constants';

describe('token management', () => {
  const typeDelay = 50;
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'aaa-testing-tokens'; // ensure they load somewhere at the top of the list
  const tokenName1 = `${cypressPrefix} token 1 ${now}`;
  const tokenName2 = `${cypressPrefix} token 2 ${now}`;
  const tokenName3 = `${cypressPrefix} token 3 ${now}`;
  const tokenID1 = `${cypressPrefix}-id-1-${now}`;
  const tokenID2 = `${cypressPrefix}-id-2-${now}`;
  const tokenID3 = `${cypressPrefix}-token-3-${now}`;
  const tokensTable = 'app-api-tokens chef-table';

  before(() => {
    cy.adminLogin('/settings/tokens').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/tokens',
        body: {
          id: tokenID1,
          name: tokenName1,
          active: true
        }
      });

      cy.request({
        auth: { bearer: admin.id_token },
        method: 'POST',
        url: '/apis/iam/v2/tokens',
        body: {
          id: tokenID2,
          name: tokenName2,
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
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['tokens']);
  });

  it('displays token table', () => {
    cy.get('#create-button').contains('Create Token');
    cy.get(tokensTable).should('exist');
    cy.get(tokensTable).contains('Name');
    cy.get(tokensTable).contains('Status');
    cy.get(tokensTable).contains('ID');
    cy.get(tokensTable).contains('Projects');
  });

  it('displays the returned tokens info', () => {
    cy.get(`${tokensTable} chef-td`).contains(tokenName1);
    cy.get(`${tokensTable} chef-td`).contains(tokenName2);

    cy.get('chef-table chef-td').contains(tokenName1).parent()
      .get('chef-td').contains('Active');
    cy.get('chef-table chef-td').contains(tokenName2).parent()
      .get('chef-td').contains('Inactive');
  });

  itFlaky('control menu has all options', () => {
    ['Copy Token', 'Toggle Status', 'Delete Token'].forEach((item, _) => {
      cy.get('chef-tbody').contains(tokenName1).parent().parent()
        .find('[data-cy=token-control]').as('controlMenu');
      cy.get('chef-tbody').contains(tokenName1).parent().parent()
        .find('[data-cy=token-control] .mat-select-trigger').as('dropdownTrigger');

      cy.get('@dropdownTrigger').should('be.visible')
        .click({ force: true }).then( () => {
          cy.get('.chef-control-menu').contains(item);
      });

    });
  });

  itFlaky('can copy token', () => {
    cy.get('chef-tbody').contains(tokenName1).parent().parent()
    .find('[data-cy=token-control]').as('controlMenu');

    cy.get('chef-tbody').contains(tokenName1).parent().parent()
      .find('[data-cy=token-control] .mat-select-trigger').as('dropdownTrigger');

    cy.get('@dropdownTrigger').should('be.visible')
      .click({ force: true }).then(() =>
        cy.get('.chef-control-menu').find('[data-cy=copy-token]').click({force: true})
      );

    cy.get('app-notification').should('be.visible');
    cy.get('app-notification').should('contain', 'API Token copied to clipboard.');
  });

  it('can delete token', () => {
    cy.get('chef-tbody').contains(tokenName1).parent().parent()
      .find('[data-cy=token-control]').as('controlMenu');

    cy.get('chef-tbody').contains(tokenName1).parent().parent()
      .find('[data-cy=token-control] .mat-select-trigger').as('dropdownTrigger');

    cy.get('@dropdownTrigger').should('be.visible')
      .click({ force: true }).then(() =>
        cy.get('.chef-control-menu').find('[data-cy=delete]').click({ force: true })
      );

    cy.get('app-delete-object-modal').find('button').contains('Delete Token')
      .click({force: true});
    cy.get('chef-tbody').contains(tokenName1).should('not.exist');
  });

  it('can create a new token', () => {
    cy.get('#create-button').contains('Create Token').click();
    cy.get('chef-modal').contains('Create Token').should('be.visible');

    cy.get('[data-cy=create-name]').focus()
      .type(tokenName3, { delay: typeDelay }).should('have.value', tokenName3);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');
    cy.get('#id-input').should('have.value', tokenID3);

    cy.get('#create-button-object-modal').click();
    cy.get('chef-modal').should('not.be.visible');
    cy.get('app-notification.info').should('be.visible');
    cy.contains(tokenName3).should('exist');
  });

  it('opens token details', () => {
    cy.get('chef-tbody').contains(tokenName2).click();
    cy.url().should('include', `/settings/tokens/${tokenID2}`);
    cy.contains(tokenName2).should('exist');
    cy.get('h1.page-title').contains(tokenName2);
    cy.get('[data-cy=token-status]').contains('Inactive');
    cy.get('[data-cy=token-id]').contains(tokenID2);
    cy.get('[data-cy=name-input]').should('have.value', tokenName2);
  });
});
