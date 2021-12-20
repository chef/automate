describe('chef server details', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const updatedServerName = `${cypressPrefix} updated server ${now}`;
  const serverID = serverName.split(' ').join('-');
  const customServerID = `${cypressPrefix}-custom-id-${now}`;
  const serverFQDN = 'chef-server-1617089723092818000.com';
  const serverIP = '176.119.50.159';
  const orgName = `${cypressPrefix} org ${now}`;
  const generatedOrgID = orgName.split(' ').join('-');
  const customOrgID = `${cypressPrefix}-custom-id-${now}`;
  const adminUser = 'test_admin_user';

  // using dummy admin key value for creating the org
  const adminKey = 'Dummy--admin--key';

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'POST',
        url: '/api/v0/infra/servers',
        body: {
          id: serverID,
          name: serverName,
          fqdn: serverFQDN,
          ip_address: serverIP
        }
      });

      cy.visit(`/infrastructure/chef-servers/${serverID}`);
      cy.get('app-welcome-modal').invoke('hide');
    });

    cy.restoreStorage();
  });

  beforeEach(() => {
    cy.restoreStorage();
  });

  afterEach(() => {
    cy.saveStorage();
  });

  describe('chef server details page', () => {
    it('displays server details', () => {
      cy.get('chef-breadcrumbs').contains('Chef Infra Servers');
      cy.get('chef-breadcrumbs').contains(serverName);
      cy.get('.page-title').contains(serverName);
      cy.get('[data-cy=add-org-button]').contains('Add Chef Organization');
    });

    it('click Update button and pressing cancel button to close slider', () => {
      cy.get('[data-cy=open-WebKey-slider]').contains('Update').click();
      cy.get('[data-cy=title]').contains('Update Web UI Key');
      cy.get('[data-cy=cancel]').click();
    });

    it('check if Upload button is disabled before entering input', () => {
      cy.get('[data-cy=open-WebKey-slider]').contains('Update').click();
      cy.get('[data-cy=title]').contains('Update Web UI Key');
      cy.get('[data-cy=Upload]')
      .invoke('attr', 'disabled')
      .then(disabled => {
      disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=Upload]').click();
    });
      cy.get('[data-cy=cancel]').click();
  });

    it('Upload Web UI Key', () => {
      cy.get('[data-cy=open-WebKey-slider]').contains('Update').click();
      cy.get('[data-cy=title]').contains('Update Web UI Key');
      cy.get('[data-cy=enter-key]').clear().type('testing');
      cy.get('[data-cy=Upload').click();
    });
  });
});
