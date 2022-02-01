describe('chef server details', () => {
  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'infra';
  const serverName = `${cypressPrefix} server ${now}`;
  const serverID = serverName.split(' ').join('-');
  const serverFQDN = 'https://ec2-18-117-112-129.us-east-2.compute.amazonaws.com';
  const serverIP = '18-117-112-129';
  const webuiKey = Cypress.env('AUTOMATE_INFRA_WEBUI_KEY').replace(/\\n/g, '\n');

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
          ip_address: serverIP,
          webui_key: webuiKey
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

    it('click Sync Org and User button and pressing cancel button to close slider', () => {
      cy.get('[data-cy=sync-org-and-users]').contains('Sync Org and Users').click();
      cy.get('[data-cy=title]').contains('Sync Org and User');
      cy.get('[data-cy=cancel]').click({multiple: true, force: true});
    });

    it('check if Upload button is disabled before entering file', () => {
      cy.get('[data-cy=sync-org-and-users]').contains('Sync Org and Users').click();
      cy.get('[data-cy=title]').contains('Sync Org and User');
      cy.get('[data-cy=upload-button]')
      .invoke('attr', 'disabled')
      .then(disabled => {
      disabled ? cy.log('buttonIsDiabled') : cy.get('[data-cy=upload-button]').click();
      });
        cy.get('[data-cy=cancel]').click({multiple: true, force: true});
    });

    it('Upload compressed file', () => {
      cy.get('[data-cy=sync-org-and-users]').contains('Sync Org and Users').click();
      cy.get('[data-cy=title]').contains('Sync Org and User');
      cy.get('[data-cy="file-input"]')
        .attachFile('infra-proxy/backup.zip');
      cy.get('[data-cy=upload-button').click();
    });
  });
});
