var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function nodeAttributeUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-attribute/_search';
}

describe("creating node attribute", function () {
  describe("one node attribute", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should return 200 with new index created", function () {
      this.timeout(4000);
      let entityUuid = 'test_id';
      let chefRun = helpers.ChefRun.clone();
      chefRun.set('entity_uuid', entityUuid);
      
      // Wait until after the chef run is posted. 
      return chakram.all([chakram.post(endpoint(), chefRun.json())]).then(function(responses) {
        expect(responses[0]).to.have.status(200);
        // Wait until the node-attribute index is refreshed.
        return chakram.all([helpers.NodeAttributeRefresh()]).then(function(responses) {
          // Wait until all the node attributes are pull from elastic search
          return chakram.all([chakram.get(nodeAttributeUrl())]).then(function(responses) {
            let response = responses[0];
            expect(response).to.have.status(200);
            expect(response.body.hits.total).to.equal(1);
            let source = response.body.hits.hits[0]._source;
            expect(source.entity_uuid).to.equal(entityUuid);
            expect(source.name).to.equal('supermarket-rhel-7-builder-45f356');
            expect(source.normal_value_count).to.equal(15);
            expect(source.default_value_count).to.equal(487);
            expect(source.override_value_count).to.equal(7);
            expect(source.automatic_value_count).to.equal(1281);
            expect(source.all_value_count).to.equal(1790);
            
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("two differnt nodes", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should create two node attribute documents in the node-attribute index", function () {
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
      
      // Wait for the post. 
      return chakram.all(postResponses).then(function(responses) {
        for (i = 0; i < responses.length; i++) { 
          let response = responses[i];
          expect(response).to.have.status(200);
        }
        // Wait for the refresh requests.
        return chakram.all([helpers.NodeAttributeRefresh()]).then(function(responses) {
          // wait for the get all docs request
          return chakram.all([chakram.get(nodeAttributeUrl())]).then(function(responses) {

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

  describe("two updates from the same node", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    // Commented out because it is being fix in separate PR.
    it("should have one document in elastic search", function () {
      this.timeout(4000);
      let entityUuid = 'test-id';
      let chefRun1 = helpers.ChefRun.clone();
      chefRun1.set('entity_uuid', entityUuid);
      chefRun1.set('chef_environment', 'first');

      let chefRun2 = helpers.ChefRun.clone();
      chefRun2.set('entity_uuid', entityUuid);
      chefRun2.set('chef_environment', 'second');
      
      // Wait for the chefRun1 post. 
      return chakram.all([chakram.post(endpoint(), chefRun1.json())]).then(function(responses) {
        let response = responses[0];
        expect(response).to.have.status(200);
        // Wait for the refresh requests.
        return chakram.all([helpers.NodeAttributeRefresh()]).then(function(responses) {
          // Wait for the chefRun2 post. 
          return chakram.all([chakram.post(endpoint(), chefRun2.json())]).then(function(responses) {
            // Wait for the refresh requests.
            return chakram.all([helpers.NodeAttributeRefresh()]).then(function(responses) {
              // wait for the get all docs request
              return chakram.all([chakram.get(nodeAttributeUrl())]).then(function(responses) {
                let response = responses[0];
                expect(response).to.have.status(200);
                expect(response.body.hits.total).to.equal(1);
                let source = response.body.hits.hits[0];
                expect(source._source.entity_uuid === entityUuid).to.be.true
              });
            });
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });
});
