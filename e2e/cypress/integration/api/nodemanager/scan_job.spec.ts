import { nodejson, uuidv4 } from '../../../support/helpers';

describe('Nodemanager config mgmt node rpc deletion', () => {

let x = 0;
  it('check 100+ nodes ', () => {
    cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/nodemanagers/id/e69dc612-7e67-43f2-9b19-256afd385820/search-nodes',
      body: {
        query: {
          filter_map: []
        }
      }
    }).then((response) => {
      x = response.body.nodes.length;
    });
  });

  it('Post 100+ nodes ', () => {
      cy.request({
        headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
        method: 'POST',
        url: 'api/v0/nodes/bulk-create',
        body: {
          nodes: nodejson
        }
      }).then((response) => {
        expect(response.body.ids).to.have.length(3);
      });

   });

   it('check 100+ nodes ', () => {
      cy.request({
      headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
      method: 'POST',
      url: 'api/v0/nodemanagers/id/e69dc612-7e67-43f2-9b19-256afd385820/search-nodes',
      body: {

          query: {
              filter_map: []
          }

      }
    }).then((response) => {
          expect(response.body.nodes).to.have.length(x + 3);
        });

   });

});

