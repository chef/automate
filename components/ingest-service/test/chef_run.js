var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

function convergeHistoryUrlWithId(runId) {
  return helpers.ELASTICSEARCH_URI + '/converge-history-*/_search?q=run_id:' + runId;
}

describe("/events/chef/run", function () {
  describe("posting unsupported message", function () {
    it("should return 500", function () {
      var response = chakram.post(endpoint(), {"username":"xyz","password":"xyz"});
      return chakram.waitFor([
        expect(response).to.have.status(501)
      ]);
    });
  });

  describe("posting example message", function () {
    it("should return 200 with new index created", function () {
      this.timeout(4000);
      let chefRun = helpers.ChefRun.clone();
      let postResponse = chakram.post(endpoint(), chefRun.json());

      // Wait for the post and the refresh requests.
      return chakram.post(endpoint(), chefRun.json()).then(function(response) {
        expect(response).to.have.status(200);
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("customizing error title of 412 \"Precondition Failed\"", function () {
    before(function () { return helpers.ESCleanupDocuments() });
    it("should return the error title of 'Error Resolving Cookbooks for Run List.'", function () {
      this.timeout(4000);
      let chefRun = helpers.ChefRunFailure.clone();
      let runId = '5f6797f7-1696-4736-89f8-a968ed945e74'
      chefRun.set('run_id', runId);
      chefRun.set('status', 'failure');

      chefRun.set('error', {
        "class": "Chef::Exceptions::EnclosingDirectoryDoesNotExist",
        "message": "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.",
        "backtrace": [],
        "description": {
          "title": "412 \"Precondition Failed\"",
          "sections": []
        }
      });

      // Wait for the post and the refresh requests.
      return chakram.post(endpoint(), chefRun.json()).then(function(response) {
        expect(response).to.have.status(200);
        return helpers.ConvergeHistoryRefresh().then(function(response) {
          return chakram.get(convergeHistoryUrlWithId(runId)).then(function(convergeHistoryResponse) {
            expect(convergeHistoryResponse).to.have.status(200);
            expect(convergeHistoryResponse.body.hits.total).to.equal(1);
            expect(convergeHistoryResponse.body.hits.hits[0]._source.error.description.title).to.equal("Error Resolving Cookbooks for Run List.");
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("customizing error title of 'Error Resolving Cookbooks for Run List:'", function () {
    before(function () { return helpers.ESCleanupDocuments() });
    it("should return the error title of 'Error Resolving Cookbooks for Run List.'", function () {
      this.timeout(4000);
      let chefRun = helpers.ChefRunFailure.clone();
      let runId = 'c0395d6e-8938-4ffd-9051-8e23a7c85f1d'
      chefRun.set('run_id', runId);
      chefRun.set('status', 'failure');

      chefRun.set('error', {
        "class": "Chef::Exceptions::EnclosingDirectoryDoesNotExist",
        "message": "file[/failed/file/resource] (insights-test::default line 26) had an error: Chef::Exceptions::EnclosingDirectoryDoesNotExist: Parent directory /failed/file does not exist.",
        "backtrace": [],
        "description": {
          "title": "Error Resolving Cookbooks for Run List:",
          "sections": []
        }
      });

      // Wait for the post and the refresh requests.
      return chakram.post(endpoint(), chefRun.json()).then(function(response) {
        expect(response).to.have.status(200);
        return helpers.ConvergeHistoryRefresh().then(function(response) {
          return chakram.get(convergeHistoryUrlWithId(runId)).then(function(convergeHistoryResponse) {
            expect(convergeHistoryResponse).to.have.status(200);
            expect(convergeHistoryResponse.body.hits.total).to.equal(1);
            expect(convergeHistoryResponse.body.hits.hits[0]._source.error.description.title).to.equal("Error Resolving Cookbooks for Run List.");
          });
        });
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("posting a bad example message", function () {
    before(function () { return helpers.ESCleanupDocuments() });

    it("should ingest the message and use the defaults instead", function () {
      let chefRunBadMessage = helpers.ChefRunBad.clone();

      // Wait for the post and the refresh requests.
      return chakram.post(endpoint(), chefRunBadMessage.json()).then(function(response) {
        expect(response).to.have.status(200);
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });
});
