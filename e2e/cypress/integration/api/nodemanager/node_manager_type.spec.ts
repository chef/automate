import { uuidv4 } from '../../../support/helpers';

describe('Nodemanager ', () => {
  describe('when it receives chef node report info', () => {
    const cypressPrefix = 'test-nodemanager-node-mgr-type';
    const clientRunsNodeId1 = uuidv4();
    const nodeName1 = `${cypressPrefix}-${Cypress.moment().format('MMDDYYhhmmss.sss')}-1`;
    before(() => {
      // Create the first node
      cy.fixture('converge/avengers1.json').then((node) => {
        const runId =  uuidv4();
        node.entity_uuid = clientRunsNodeId1;
        node.node_name = nodeName1;
        node.id = runId;
        node.run_id = runId;

        cy.sendToDataCollector(node);
      });

      // Wait for that config mgmt node to appear
      cy.waitForClientRunsNode(clientRunsNodeId1);

      // Wait for the node to appear in the node manager
      cy.waitForNodemanagerNode(clientRunsNodeId1);
    });
    // get the nodemanager node
    it('sets the manager to chef', () => {
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'GET',
            url: `api/v0/nodes/id/${clientRunsNodeId1}`
        }).then((resp: Cypress.ObjectLike) => {
            console.log(resp.body);
            expect(resp.body.manager).to.equal('chef');
        });
    });
  });
});
