describe('Node manager service ', () => {

  if (Cypress.env('IS_PROXY_ENV') === 'true') {
    let id: any;
    let nodeId: any;

    it('add secret', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/secrets',
        body: {
          id: 'test',
          name: 'test',
          type: 'ssh',
          data: [
            {key: 'username', value: 'root'},
            {key: 'password', value: '123456'},
            {key: 'key', value: null}
          ],
          tags: []
        }
      }).then((response) => {
        id = response.body.id;
      });
    });

    it('add node', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/nodes/bulk-create',
        body: {
          nodes: [{
            name: 'test-localhost',
            manager: 'automate',
            target_config: {
            backend: 'ssh',
            secrets: [id],
            port: 22,
            sudo: false,
            hosts: ['localhost']
            },
            tags: []
          }]
        }
      }).then((response) => {
        nodeId = response.body.ids[0];
      });
    });

    it('upload file', () => {
      cy.uploadFileRequest(
        'testproxy-0.1.0.tar.gz',
        'testproxy-0.1.0.tar.gz',
        'demoFileUploadRequest'
      );

      cy.wait('@demoFileUploadRequest', {timeout: 120000}).then((response) => {
        expect(response.status).to.eq(200);
      });
    });

    it('get manager id', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/nodemanagers/search',
        body: {filter_map: [{key: 'manager_type', values: ['automate']}], sort: 'date_added'}
      }).then((response) => {
        id = response.body.managers[0].id;
      });
    });

    it('run scan', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/compliance/scanner/jobs',
        body: {
            type: 'exec',
            tags: [],
            name: 'test',
            profiles: ['compliance://admin/testProxy#0.1.0'],
            node_selectors: [{
              manager_id: id,
              filters: [{
                key: 'name',
                values: ['test-localhost'],
                exclude: false
              }]
            }],
            recurrence: ''
        }
      }).then((response) => {
        id = response.body.id;
      });
    });

    it('wait for scan', () => {
      const endDate = Cypress.moment().utc().endOf('day');
      const startDate = Cypress.moment(endDate).subtract(10, 'days').startOf('day');
      cy.wait(90000);
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/compliance/reporting/stats/summary',
        body: {
          filters: [{
            type: 'job_id',
            values: [id]},
          {
            type: 'start_time',
            values: [startDate]}, {type: 'end_time', values: [endDate]}]}
      }).then((response) => {
        id = response.body.id;
        expect(response.body.report_summary.status).to.eq('passed');
      });
    });
  }
});

declare global {
  interface Window {
    XMLHttpRequest: any;
  }
}

Cypress.Commands.add('uploadFileRequest', (fileToUpload: any, uniqueName: any, aliasName: any) => {
  const data = new FormData();
  cy.server()
    .route({
      method: 'POST',
      url: 'api/v0/compliance/profiles?contentType=application/x-gzip&owner=admin'
    })
    .as(aliasName)
    .window()
    .then((win) => {
      cy.fixture(fileToUpload, 'binary')
        .then((binary) => Cypress.Blob.binaryStringToBlob(binary))
        .then((blob) => {
          const xhr = new win.XMLHttpRequest();
          data.set('file', blob, fileToUpload);
          xhr.open('POST', 'api/v0/compliance/profiles?contentType=application/x-gzip&owner=admin');
          xhr.setRequestHeader('api-token', Cypress.env('ADMIN_TOKEN'));
          xhr.timeout = 300000;
          xhr.send(data);
        });
    });
});
