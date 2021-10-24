describe('chef datafeed', () => {

  const token = 'behwveh3238238=';
  const DestinationID = 'chef-server-dev-test';
  const reusableDate = Date.now();
  let adminIdToken = '';
  const name = 'cytest' + reusableDate;
  const url = 'https://localhost/api/x_chef_automate/asset';
  const ServiceNow = 'Service Now';
  const Webhook = 'Webhook';

  before(() => {
      cy.adminLogin('/').then(() => {
          const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
          adminIdToken = admin.id_token;
          cy.get('app-welcome-modal').invoke('hide');
          cy.restoreStorage();
      });
      cy.visit('/compliance/scan-jobs/nodes/add');
      cy.get('app-welcome-modal').invoke('hide');
      // cy.restoreStorage();
    });

  beforeEach(() => {
      cy.restoreStorage();
    });

  afterEach(() => {
      cy.saveStorage();
  });

  describe ('add node in scan jobs', () => {
    before(() => {
      cy.request({
      auth: { bearer: adminIdToken },
      method: 'POST',
      url: '/api/v0/secrets/search',
      body: {}
      }).then((secrectSearchResp) => {
        if (!(secrectSearchResp.body.total >= 300)) {
          for (let i = 0; i < 600; i++) {
            cy.request({
              auth: { bearer: adminIdToken },
              method: 'POST',
              url: '/api/v0/secrets',
              body: {
                'name': i % 2 === 0 ? 'secret-ssh' + i : 'secret-winrm' + i,
                'type': i % 2 === 0 ? 'ssh' : 'winrm',
                'data': [
                  {
                    'key': 'username',
                    'value': 'username'
                  },
                  {
                    'key': 'password',
                    'value': 'password'
                  },
                  {
                    'key': 'key',
                    'value': null
                  }
                ],
                'tags': []
              }
            }).then((secrectResp) => {
              expect(secrectResp.status).to.equal(200);
              expect(secrectResp.body).to.property('id');
            });
          }
        }
      });
    });

    it('Add 1 host', () => {
      cy.get('[data-cy=add-nodes]').contains('Add 0 Node(s)');
      cy.get('[data-cy=hosts]').type(name);
      cy.get('[data-cy=add-nodes]').contains('Add 1 Node(s)');
    });

    it('Add multipule hosts', () => {
      cy.get('[data-cy=hosts]').type(`,${name}-1,${name}-2`);
      cy.get('[data-cy=add-nodes]').contains('Add 3 Node(s)');
    });

    it('open accordion', () => {
      cy.get('[data-cy=cred-accordion]').click();
    });

    it('select credentials type from dropdown', () => {
      cy.get('[data-cy=selectCredtype]').click();
      cy.get('[data-cy=ssh]').click();
    });

    it('select credentials from select box', () => {
      cy.get('[data-cy=rightSide-1]').click();
      cy.get('[data-cy=rightSide-0]').click();
      cy.get('[data-cy=rightSide-5]').click();
    });

    it('should scroll to bottom', () => {
      cy.get('[id=scroll-right-side]').scrollTo('bottom', {duration: 1000});
    });

    it('should scroll to top, select 101 and 201 index data', () => {
      cy.get('[id=scroll-right-side]').scrollTo('top', {duration: 1000});
      cy.get('[data-cy=rightSide-101]').click();
      cy.get('[id=scroll-right-side]').scrollTo('bottom', {duration: 1000});
      cy.get('[data-cy=rightSide-201]').click();
    });

    it('unselect credentials which is already selected from select box', () => {
      cy.get('[id=scroll-right-side]').scrollTo('top', {duration: 1000});
      cy.get('[data-cy=rightSide-0]').click();
      cy.get('[data-cy=rightSide-1]').click();
      cy.get('[data-cy=rightSide-1]').click();
    });

    it('send selected data to right side', () => {
      cy.get('[data-cy=right-side-button]').click();
    });

    it(
      'select cred type from dropdown and already selected should not present on left side', () => {
      cy.get('[data-cy=selectCredtype]').click();
      cy.get('[data-cy=ssh]').click();
      cy.get('[data-cy=rightSide-5]').should('not.have.value', 'secret-ssh106');
    });

    it(
      'port should be 22 if i select shh and enable sudo should be visible', () => {
      cy.get('[data-cy=port]').should('have.value', '22');
      cy.get('[data-cy=port]').should('not.have.value', '5985');
      cy.get('[data-cy=self_signed]').should('not.be.visible');
      cy.get('[data-cy=ssl]').should('not.be.visible');
      cy.get('[data-cy=sudo]').should('be.visible');
    });

    it(
      'port should be 5985 if i select WinRM and ssl and self signed should present', () => {
        cy.get('[id=scroll-right-side]').scrollTo('top');
        cy.get('[data-cy=selectCredtype]').click();
        // cy.get('[data-cy=ssh]').click();
        cy.get('[data-cy=winrm]').should('be.visible').click();
        // cy.get('[data-cy=port]').should('have.value', '5985');
        // cy.get('[data-cy=port]').should('not.have.value', '22');
        // cy.get('[data-cy=self_signed]').should('be.visible');
        // cy.get('[data-cy=ssl]').should('be.visible');
        // cy.get('[data-cy=sudo]').should('not.be.visible');
    });


    it('click on Add nodes', () => {
      cy.get('[data-cy=add-nodes]').click();
    });

  });

});
