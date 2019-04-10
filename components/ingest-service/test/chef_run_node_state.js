var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function nodeStateUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-state/_search';
}

describe("creating node state", function () {
  describe("one node state doc", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should return 200 with new index created", function () {
      this.timeout(4000);
      let entityUuid = 'test_id';
      let endTime = '2016-06-28T15:13:22Z';
      let chefRun = helpers.ChefRun.clone();
      chefRun.set('entity_uuid', entityUuid);
      chefRun.set('end_time', endTime);
      
      // Wait for the post requests. 
      return chakram.all([chakram.post(endpoint(), chefRun.json())]).then(function(responses) {
        expect(responses[0]).to.have.status(200);
        // Wait for the refresh requests.
        return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
          // Wait for the get all docs request
          return chakram.all([chakram.get(nodeStateUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200)
            expect(response.body.hits.total).to.equal(1);
            let source = response.body.hits.hits[0]._source
            expect(source.entity_uuid).to.equal(entityUuid);
            expect(source.exists).to.equal(true);
            expect(source.checkin).to.equal(endTime);
          });
        });  
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("two differnt nodes", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should create two node state documents in the node-state index", function () {
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
        return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
          // wait for pulling all the node-state documents.
          return chakram.all([chakram.get(nodeStateUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(2);
            let sources = response.body.hits.hits;
            expect(sources[0]._source.entity_uuid === entityUuid1 || sources[0]._source.entity_uuid === entityUuid2).to.be.true;
            expect(sources[1]._source.entity_uuid === entityUuid1 || sources[1]._source.entity_uuid === entityUuid2).to.be.true;
            expect(sources[1]._source.entity_uuid === sources[0]._source.entity_uuid).to.be.false;
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("two updates from the same node", function () {
    before(function () { return helpers.ESCleanupDocuments() });
    // Commented out because it is being fix in separate PR. 
    it("should have one document in elastic search", function () {
      this.timeout(4000);
      let entityUuid = 'test-id';
      let runID1 = 'run_id1';
      let runID2 = 'run_id2';
      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid);
      chefRun1.set('status', 'failure');
      chefRun1.set('run_id', runID1);

      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid);
      chefRun2.set('status', 'success');
      chefRun2.set('run_id', runID2);
      
      // Wait for the chefRun1 post. 
      return chakram.all([chakram.post(endpoint(), chefRun1.json())]).then(function(responses) {
        let response = responses[0];
        expect(response).to.have.status(200);
        // Wait for the refresh requests.
        return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
          // wait for the get all docs request
          return chakram.all([chakram.get(nodeStateUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(1);
            let source = response.body.hits.hits[0]._source;
            expect(source.entity_uuid).to.equal(entityUuid);
            expect(source.status).to.equal('failure');
            expect(source.latest_run_id).to.equal(runID1);
            // Wait for the chefRun2 post. 
            return chakram.all([chakram.post(endpoint(), chefRun2.json())]).then(function(responses) {
              // Wait for the refresh requests.
              return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
                // wait for the get all docs request
                return chakram.all([chakram.get(nodeStateUrl())]).then(function(responses) {
                  let response = responses[0];
                  expect(response).to.have.status(200);
                  expect(response.body.hits.total).to.equal(1);
                  let source = response.body.hits.hits[0]._source;
                  expect(source.entity_uuid).to.equal(entityUuid);
                  expect(source.status).to.equal('success');
                  expect(source.latest_run_id).to.equal(runID2);
                });
              });
            });
          });
        });
      });
    });

    // after(function () { return helpers.ESCleanupDocuments() });
  });
});
