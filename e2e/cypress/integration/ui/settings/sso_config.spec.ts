describe('sso config', () => {
  const now = Cypress.moment().format('MMDDYYhhmm');
  let adminIdToken = '';
  const cypressPrefix = 'sso-config';
  let deploymentType = '';
  const ssoUrl = "https://chef.okta.com/app/sso/saml";
  const emailAttr = "email123";
  const usernameAttr = "email";
  const entityIssuer = "http://automate.chef.co/dex/callback";
  const caInfo = `-----BEGIN CERTIFICATE-----
  MIIDnjCCAoagAwIBAgIGAUtB26KcMA0GCSqGSIb3DQEBBQUAMIGPMQswCQYDVQQGEwJVUzETMBEG
  -----END CERTIFICATE-----`;
  let getSsoUrl= "";

  before(() => {
    cy.adminLogin('/settings/sso-config').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      console.log("admin: ", admin);
      adminIdToken = admin.id_token;

      cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users']);

      cy.request({
        auth: { bearer: adminIdToken },
        method: 'GET',
        url: `/api/v0/telemetry/config`,
      }).then(resp => {
        deploymentType = resp.body.deployment_type;
        if (deploymentType === 'SAAS') {
          cy.request({
            auth: { bearer: adminIdToken },
            method: 'GET',
            url: `/api/v0/sso/config`,
          }).then(resp => {
            getSsoUrl = resp.body.sso_url;
          })
        }
      })
    });

    cy.restoreStorage();
    cy.viewport(1080, 880);
  });

  beforeEach(() => {
    cy.restoreStorage();
  });
  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['users']);
  });

  it('check sso form field validations', () => {
    if (deploymentType === 'SAAS') {
      if(getSsoUrl !== '') {
        cy.get('.remove-button').contains('Remove Configuration').should('be.enabled');
      } else {
        cy.get('.remove-button').contains('Remove Configuration').should('be.disabled');
      }
      cy.get('#ssoUrl').type('test').clear();
      cy.get('app-sso-config .page-body chef-error').contains('SSO URL is required.').should('be.visible');
      cy.get('#ssoUrl').clear().type('abcd');
      cy.get('app-sso-config .page-body chef-error').contains('SSO URL is invalid.').should('be.visible');
      cy.get('#ssoUrl').clear().type(ssoUrl);

      cy.get('#emailAttribute').type('test').clear();
      cy.get('app-sso-config .page-body chef-error').contains('Email Attribute is required.').should('be.visible');
      cy.get('#emailAttribute').clear().type('abcd');
      cy.get('app-sso-config .page-body chef-error').contains('Email Attribute must be at least 5 characters.').should('be.visible');
      cy.get('#emailAttribute').clear().type(emailAttr);

      cy.get('#usernameAttribute').type('test').clear();
      cy.get('app-sso-config .page-body chef-error').contains('Username Attribute is required.').should('be.visible');
      cy.get('#usernameAttribute').clear().type('abcd');
      cy.get('app-sso-config .page-body chef-error').contains('Username Attribute must be at least 5 characters.').should('be.visible');
      cy.get('#usernameAttribute').clear().type(usernameAttr);

      cy.get('#entityIssuer').type('test').clear();
      cy.get('app-sso-config .page-body chef-error').contains('Entity Issuer is required.').should('be.visible');
      cy.get('#entityIssuer').clear().type('abcd');
      cy.get('app-sso-config .page-body chef-error').contains('Entity Issuer is invalid.').should('be.visible');
      cy.get('#entityIssuer').clear().type(entityIssuer);

      cy.get('#caInfo').type('test').clear();
      cy.get('app-sso-config .page-body chef-error').contains('CA information is required.').should('be.visible');
      cy.get('#submit').contains('Submit').should('be.disabled');
      cy.get('#caInfo').clear().type(caInfo);

      cy.get('#submit').contains('Submit').should('be.enabled');
    } else {
      cy.get('.page-body form').should('be.undefined');
    }
  })
});
