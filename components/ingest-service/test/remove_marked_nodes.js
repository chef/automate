var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function chefRunEndpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function chefActionEndpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/action';
}

function nodeAttributeUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-attribute/_search';
}

function nodeStateUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-state/_search';
}

function convergeHistoryUrl() {
  return helpers.ELASTICSEARCH_URI + '/converge-history-*/_search';
}

function deleteNodeSchedulerUrl() {
  return helpers.REST_SERVICE_URI + '/job/delete-nodes/';
}

describe("remove marked nodes", function () {
  describe("remove one node", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("mark a node then delete all docs associated to the node, but leave others", function () {
      this.timeout(4000);
      let entityUuid1 = 'run_1';
      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid1);

      let entityUuid2 = 'run_2';
      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid2);

      let entityUuid3 = 'run_3';
      let chefRun3 = helpers.ChefRun.clone();
      chefRun3.set('entity_uuid', entityUuid3);

      let chefAction = helpers.ChefAction.clone();
      chefAction.set('node_id', entityUuid2);
      chefAction.set('task', 'delete');

      // not sure why I need to do the cleanup in the test? 
      return chakram.all([helpers.ESCleanupDocuments(),
        // configure the delete scheduler to not have a threshold
        chakram.post(deleteNodeSchedulerUrl() + 'config?every=1m&threshold=0m&running=false')]).then(function(responses) {
        expect(responses[1]).to.have.status(200);
        // create three different nodes
        return chakram.all([
          chakram.post(chefRunEndpoint(), chefRun1.json()),
          chakram.post(chefRunEndpoint(), chefRun2.json()),
          chakram.post(chefRunEndpoint(), chefRun3.json()) ]).then(function(responses) {
            expect(responses.length).to.equal(3);
            expect(responses[0]).to.have.status(200);
            expect(responses[1]).to.have.status(200);
            expect(responses[2]).to.have.status(200);
            return chakram.all([
              helpers.NodeStateRefresh(), 
              helpers.NodeAttributeRefresh(), 
              helpers.ConvergeHistoryRefresh()]).then(function(responses) {
              // mark a node for delete
              return chakram.post(chefActionEndpoint(), chefAction.json()).then(function(response) {
                expect(response).to.have.status(200);
                return chakram.all([
                  helpers.NodeStateRefresh(), 
                  helpers.NodeAttributeRefresh(), 
                  helpers.ConvergeHistoryRefresh()]).then(function(responses) {
                  // send remove marked nodes
                  return chakram.post(deleteNodeSchedulerUrl() + 'delete-marked-nodes').then(function(response) {
                    expect(response).to.have.status(200);

                    return chakram.all([
                      helpers.NodeStateRefresh(), 
                      helpers.NodeAttributeRefresh(), 
                      helpers.ConvergeHistoryRefresh()]).then(function(responses) {
                      return chakram.all([
                        chakram.get(nodeStateUrl()), 
                        chakram.get(nodeAttributeUrl()),
                        chakram.get(convergeHistoryUrl())
                        ]).then(function(responses) {
                        // ensure only one of the docs for each index is removed.
                        let nodeStateResponse = responses[0];
                        let nodeAttributeResponse = responses[1];
                        let convergeHistoryResponse = responses[2];

                        expect(nodeStateResponse).to.have.status(200);
                        expect(2).to.equal(nodeStateResponse.body.hits.total);

                        let source1 = nodeStateResponse.body.hits.hits[0]._source
                        let source2 = nodeStateResponse.body.hits.hits[1]._source
                        expect(source1.entity_uuid === entityUuid1 || source2.entity_uuid === entityUuid1).to.equal(true);
                        expect(source1.entity_uuid === entityUuid3 || source2.entity_uuid === entityUuid3).to.equal(true);
                        
                        expect(convergeHistoryResponse).to.have.status(200);
                        expect(convergeHistoryResponse.body.hits.total).to.equal(2);

                        expect(nodeAttributeResponse).to.have.status(200);
                        expect(nodeAttributeResponse.body.hits.total).to.equal(2);
                      });
                    });
                  });
                });
              });
            });
          });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("remove two node", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("mark two nodes then delete all docs associated to the nodes, but leave the other one", function () {
      this.timeout(4000);
      let entityUuid1 = 'run_1';
      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid1);

      let entityUuid2 = 'run_2';
      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid2);

      let entityUuid3 = 'run_3';
      let chefRun3 = helpers.ChefRun.clone();
      chefRun3.set('entity_uuid', entityUuid3);

      let chefAction1 = helpers.ChefAction.clone();
      chefAction1.set('node_id', entityUuid1);
      chefAction1.set('task', 'delete');

      let chefAction2 = helpers.ChefAction.clone();
      chefAction2.set('node_id', entityUuid2);
      chefAction2.set('task', 'delete');

      // create three different nodes
      return chakram.all([
        chakram.post(chefRunEndpoint(), chefRun1.json()),
        chakram.post(chefRunEndpoint(), chefRun2.json()),
        chakram.post(chefRunEndpoint(), chefRun3.json()),
        // configure the delete scheduler to not have a threshold
        chakram.post(deleteNodeSchedulerUrl() + 'config?every=1m&threshold=0m&running=false')]).then(function(responses) {
        expect(responses.length).to.equal(4);
        expect(responses[0]).to.have.status(200);
        expect(responses[1]).to.have.status(200);
        expect(responses[2]).to.have.status(200);
        expect(responses[3]).to.have.status(200);
        return chakram.all([
          helpers.NodeStateRefresh(), 
          helpers.NodeAttributeRefresh(), 
          helpers.ConvergeHistoryRefresh()]).then(function(responses) {
          // mark a node for delete
          return chakram.all([
            chakram.post(chefActionEndpoint(), chefAction1.json()), 
            chakram.post(chefActionEndpoint(), chefAction2.json())]).then(function(responses) {
            expect(responses[0]).to.have.status(200);
            expect(responses[1]).to.have.status(200);
            return chakram.all([
              helpers.NodeStateRefresh(), 
              helpers.NodeAttributeRefresh(), 
              helpers.ConvergeHistoryRefresh()]).then(function(responses) {
              // send remove marked nodes
              return chakram.post(deleteNodeSchedulerUrl() + 'delete-marked-nodes').then(function(response) {
                expect(response).to.have.status(200);

                return chakram.all([
                  helpers.NodeStateRefresh(), 
                  helpers.NodeAttributeRefresh(), 
                  helpers.ConvergeHistoryRefresh()]).then(function(responses) {
                  return chakram.all([
                    chakram.get(nodeStateUrl()), 
                    chakram.get(nodeAttributeUrl()),
                    chakram.get(convergeHistoryUrl())
                    ]).then(function(responses) {
                    // ensure only one of the docs for each index is removed.
                    let nodeStateResponse = responses[0];
                    let nodeAttributeResponse = responses[1];
                    let convergeHistoryResponse = responses[2];

                    expect(nodeStateResponse).to.have.status(200);
                    expect(nodeStateResponse.body.hits.total).to.equal(1);

                    let source = nodeStateResponse.body.hits.hits[0]._source
                    expect(source.entity_uuid).to.equal(entityUuid3);
                    
                    expect(nodeAttributeResponse).to.have.status(200);
                    expect(nodeAttributeResponse.body.hits.total).to.equal(1);

                    expect(convergeHistoryResponse).to.have.status(200);
                    expect(convergeHistoryResponse.body.hits.total).to.equal(1);
                  });
                });
              });
            });
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("do not remove nodes not passed threshold", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("mark node but don't wait for threshold to pass, ensure node is not removed", function () {
      this.timeout(4000);
      let entityUuid = 'run_1';
      let chefRun = helpers.ChefRun.clone();
      chefRun.set('entity_uuid', entityUuid);

      let chefAction = helpers.ChefAction.clone();
      chefAction.set('node_id', entityUuid);
      chefAction.set('task', 'delete');

      // add one node
      // configure the delete scheduler to have a threshold of a day
      return chakram.post(deleteNodeSchedulerUrl() + 'config?every=1m&threshold=1d&running=false').then(function(response) {
      // add one node
        return chakram.post(chefRunEndpoint(), chefRun.json()).then(function(response) {
          expect(response).to.have.status(200);
          return chakram.all([
            helpers.NodeStateRefresh(), 
            helpers.NodeAttributeRefresh(), 
            helpers.ConvergeHistoryRefresh()]).then(function(responses) {
            // mark the node
            return chakram.post(chefActionEndpoint(), chefAction.json()).then(function(response) {
              return chakram.all([
                helpers.NodeStateRefresh(), 
                helpers.NodeAttributeRefresh(), 
                helpers.ConvergeHistoryRefresh()]).then(function(responses) {
                  // send remove marked nodes
                return chakram.post(deleteNodeSchedulerUrl() + 'delete-marked-nodes').then(function(response) {
                  expect(response).to.have.status(200);
                  
                  return chakram.all([
                    helpers.NodeStateRefresh(), 
                    helpers.NodeAttributeRefresh(), 
                    helpers.ConvergeHistoryRefresh()]).then(function(responses) {
                    return chakram.all([
                      chakram.get(nodeStateUrl()), 
                      chakram.get(nodeAttributeUrl()),
                      chakram.get(convergeHistoryUrl())
                      ]).then(function(responses) {
                      // ensure no docs are removed
                      let nodeStateResponse = responses[0];
                      let nodeAttributeResponse = responses[1];
                      let convergeHistoryResponse = responses[2];

                      expect(nodeStateResponse).to.have.status(200);
                      expect(nodeStateResponse.body.hits.total).to.equal(1);

                      let source = nodeStateResponse.body.hits.hits[0]._source
                      expect(source.entity_uuid).to.equal(entityUuid);
                      
                      expect(nodeAttributeResponse).to.have.status(200);
                      expect(nodeAttributeResponse.body.hits.total).to.equal(1);

                      expect(convergeHistoryResponse).to.have.status(200);
                      expect(convergeHistoryResponse.body.hits.total).to.equal(1);
                    });
                  });
                });
              });
            });
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

});
