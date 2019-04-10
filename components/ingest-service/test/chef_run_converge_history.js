var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function convergeHistoryUrl() {
  return helpers.ELASTICSEARCH_URI + '/converge-history-*/_search';
}

describe("creating converge history", function () {
  describe("one converge history", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should return 200 with new index created and correct data", function () {
      this.timeout(4000);
      let entityUuid = 'test_id';
      let nodeName = 'bob';
      let runID = 'run_id';
      let chefRun = helpers.ChefRun.clone();
      chefRun.set('entity_uuid', entityUuid);
      chefRun.set('node_name', nodeName);
      chefRun.set('run_id', runID);
      
      // Wait until after the chef run is posted. 
      return chakram.all([chakram.post(endpoint(), chefRun.json())]).then(function(responses) {
        expect(responses[0]).to.have.status(200);
        // Wait until the converge-history index is refreshed.
        return chakram.all([helpers.ConvergeHistoryRefresh()]).then(function(responses) {
          // Wait until all the node attributes are pull from elastic search
          return chakram.all([chakram.get(convergeHistoryUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(1);
            let source = response.body.hits.hits[0]._source;
            expect(source.entity_uuid).to.equal(entityUuid);
            expect(source.node_name).to.equal(nodeName);
            expect(source.run_id).to.equal(runID);
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("two differnt nodes", function () {

    before(function () { return helpers.ESCleanupDocuments() });

    it("should create two converge history documents in the converge history index", function () {
      this.timeout(4000);
      let entityUuid1 = 'id1';
      let entityUuid2 = 'id2';
      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid1);

      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid2);

      let postResponses = [
        chakram.post(endpoint(), chefRun1.json()), 
        chakram.post(endpoint(), chefRun2.json())
      ];
      
      // Wait to post both runs to pipeline 
      return chakram.all(postResponses).then(function(responses) {
        for (i = 0; i < responses.length; i++) { 
          let response = responses[i];
          expect(response).to.have.status(200);
        }
        // Wait for the refreshing the node-state index.
        return chakram.all([helpers.ConvergeHistoryRefresh()]).then(function(responses) {
          // wait for pulling all the node-state documents.
          return chakram.all([chakram.get(convergeHistoryUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(2);
            let sources = response.body.hits.hits;
            expect(sources[0]._source.entity_uuid === entityUuid1 || sources[0]._source.entity_uuid === entityUuid2).to.be.true
            expect(sources[1]._source.entity_uuid === entityUuid1 || sources[1]._source.entity_uuid === entityUuid2).to.be.true
            expect(sources[1]._source.entity_uuid === sources[0]._source.entity_uuid).to.be.false
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("two runs from the same node", function () {

    before(function () { return helpers.ESCleanupDocuments() });

    it("should create two converge history documents in the converge history index", function () {
      this.timeout(8000);
      let entityUuid = 'id1';
      let runID1 = 'runId1';
      let runID2 = 'runId2';

      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid);
      chefRun1.set('run_id', runID1);
      chefRun1.set('start_time', '2016-06-28T15:13:21Z');
      chefRun1.set('end_time', '2016-06-28T15:13:22Z');

      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid);
      chefRun2.set('run_id', runID2);
      chefRun1.set('start_time', '2016-06-29T15:13:21Z');
      chefRun1.set('end_time', '2016-06-29T15:13:22Z');

      let postResponses = [
        chakram.post(endpoint(), chefRun1.json()), 
        chakram.post(endpoint(), chefRun2.json())
      ];
      
      // Wait to post both runs to pipeline 
      return chakram.all(postResponses).then(function(responses) {
        for (i = 0; i < responses.length; i++) { 
          let response = responses[i];
          expect(response).to.have.status(200);
        }
        // Wait for the refreshing the node-state index.
        return chakram.all([helpers.ConvergeHistoryRefresh()]).then(function(responses) {
          // wait for pulling all the node-state documents.
          return chakram.all([chakram.get(convergeHistoryUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(2);
            let sources = response.body.hits.hits;
            expect(sources[0]._source.entity_uuid).to.equal(entityUuid);
            expect(sources[1]._source.entity_uuid).to.equal(entityUuid);
            expect(sources[0]._source.run_id === runID1 || sources[0]._source.run_id === runID2).to.be.true
            expect(sources[1]._source.run_id === runID1 || sources[1]._source.run_id === runID2).to.be.true
            expect(sources[1]._source.run_id === sources[0]._source.run_id).to.be.false
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });
});
