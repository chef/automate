var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/liveness';
}

function runEndpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function nodeStateUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-state/_search';
}


describe("/events/chef/liveness", function () {
  describe("posting malformed message", function () {
    it("should return 400", function () {
      var response = chakram.post(endpoint(), {"username":"xyz","password":"xyz"});
      return chakram.waitFor([
        expect(response).to.have.status(400)
      ]);
    });
  });

  describe("posting example message", function () {

    it("should return 200 with new index created", function () {
      this.timeout(4000);
      let ChefLiveness = helpers.ChefLiveness.clone();

      // Wait for the post and the refresh requests.
      return chakram.post(endpoint(), ChefLiveness.json()).then(function(response) {
        expect(response).to.have.status(200);
      });
    });

    it("results in a non existant node", function () {
      this.timeout(4000);
      let ChefLiveness = helpers.ChefLiveness.clone();
      let postResponse = chakram.post(endpoint(), ChefLiveness.json());

      // Wait for the post and the refresh requests.
      return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
        // Wait for the get all docs request
        return chakram.get(nodeStateUrl()).then(function(response) {
          expect(response).to.have.status(200)
          let source = response.body.hits.hits[0]._source
          expect(source.entity_uuid).to.equal("0271e125-97dd-498a-b026-8448ee60aafe");
          expect(source.exists).to.not.equal(true);
          expect(source.liveness_managed).to.equal(true);
        });
      });
    });
    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("posting CCR followed by liveness message", function () {

    it("results in an existing node that is liveness managed", function () {
      this.timeout(4000);
      let ChefLiveness = helpers.ChefLiveness.clone();
      let ChefRun = helpers.ChefRun.clone();
      // post liveness
      return chakram.post(runEndpoint(), ChefRun.json()).then(function(firstResponse) {
      // post CCR
      return chakram.post(endpoint(), ChefLiveness.json()).then(function(responses) {
        // Wait for the post and the refresh requests.
        return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
          // Wait for the get all docs request
          return chakram.get(nodeStateUrl()).then(function(response) {
            expect(firstResponse).to.have.status(200)
            expect(response).to.have.status(200)
            let source = response.body.hits.hits[0]._source
            expect(source.entity_uuid).to.equal("0271e125-97dd-498a-b026-8448ee60aafe");
            expect(source.liveness_managed).to.equal(true);
            expect(source.exists).to.equal(true);

          });
        });
      });
    });
    });
    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("posting liveness message followed by CCR", function () {
    it("results in an existing node that is liveness managed", function () {
      this.timeout(4000);
      let ChefLiveness = helpers.ChefLiveness.clone();
      let ChefRun = helpers.ChefRun.clone();
      // post liveness
      let postResponse = chakram.post(endpoint(), ChefLiveness.json());
      // post CCR
      return chakram.post(runEndpoint(), ChefRun.json()).then(function(responses) {
        // Wait for the post and the refresh requests.
        return chakram.all([helpers.NodeStateRefresh()]).then(function(responses) {
          // Wait for the get all docs request
          return chakram.get(nodeStateUrl()).then(function(response) {
            expect(response).to.have.status(200)
            let source = response.body.hits.hits[0]._source
            expect(source.entity_uuid).to.equal("0271e125-97dd-498a-b026-8448ee60aafe");
            expect(source.liveness_managed).to.equal(true);
            expect(source.exists).to.equal(true);

          });
        });
      });
    });
    after(function () { return helpers.ESCleanupDocuments() });
  });

});
