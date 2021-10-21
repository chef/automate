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
      // before(() => {
      //   for (let i = 0; i < 100; i++) {
      //     cy.request({
      //         auth: { bearer: adminIdToken },
      //         method: 'POST',
      //         url: '/api/v0/secrets',
      //         body: {
      //           'name': i % 2 === 0 ? 'secret-ssh' : 'secret-winrm',
      //           'type': i % 2 === 0 ? 'ssh' : 'winrm',
      //           'data': [
      //             {
      //               'key': 'username',
      //               'value': 'username'
      //             },
      //             {
      //               'key': 'password',
      //               'value': 'password'
      //             },
      //             {
      //               'key': 'key',
      //               'value': null
      //             }
      //           ],
      //           'tags': []
      //         }
      //     }).then((secrectResp) => {
      //           expect(secrectResp.status).to.equal(200);
      //           expect(secrectResp.body).to.property('id');
      //     });
      //   }
      // });

      it('select credentials from select box', () => {
        cy.get('[data-cy=cred-accordion]').click();
        cy.get('[data-cy=rightSide-1]').click();
        cy.get('[data-cy=rightSide-0]').click();
        cy.get('[data-cy=rightSide-5]').click();
    });

    it('should scroll bottom', () => {
      cy.get('[id=scroll-right-side]').scrollTo('bottom', {duration: 1000});
    });

    it('should scroll top to select 101 index data and select 201 index data', () => {
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

  it('send selected data to left side', () => {
    cy.get('[data-cy=right-side-button]').click();
  });

  it('send selected data to left side', () => {
    cy.get('[data-cy=right-side-button]').click();
  });

  });

});
