var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/action';
}

function nodeStateUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-state/_search';
}

function actionsUrl() {
  return helpers.ELASTICSEARCH_URI + '/actions-*/_search';
}

function chefRunEndpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

describe("/events/chef/action", function () {
  describe("posting unsupported message", function () {
    it("should return 500 with empty json", function () {
      var response = chakram.post(endpoint(), {"username":"xyz","password":"xyz"});
      return chakram.waitFor([
        expect(response).to.have.status(501)
      ]);
    });
  });

  describe("posting example message no elastic search data", function () {
    // Because as soon as a Node is created it can be deleted, even if it's never been run, there's
    // a possibility automate might never know about it, and therefore we need it to not throw an
    // error if it has no record of the node we're trying to delete.
    it("should return 200", function () {
      let chefAction = helpers.ChefAction.clone();

      // Wait for the post and the refresh requests.
      return chakram.all([chakram.post(endpoint(), chefAction.json())]).then(function(responses) {
        expect(responses[0]).to.have.status(200);
      });
    });

    after(function () { return helpers.ESCleanupDocuments() });
  });

  describe("posting example message with elastic search data", function () {
    describe("actions with self updates are dropped", function () {
      before(function () { return helpers.ESCleanupDocuments() });
      it("should return 200 and not persist action", function () {
        this.timeout(4000);

        let ChefCCRAction = helpers.ChefCCRAction.clone();

        return chakram.post(endpoint(), ChefCCRAction.json()).then(function(response) {
          expect(response).to.have.status(200)
          return helpers.ActionRefresh().then(function(response) {
            return chakram.get(actionsUrl()).then(function(response) {
              expect(response).to.have.status(200);
              expect(response.body.hits.total).to.equal(0);
            });
          });
        });
      });
    });

    describe("actions with user updates are persisted", function () {
      before(function () { return helpers.ESCleanupDocuments() });
      it("should return 200 and persist action", function () {
        this.timeout(4000);

        let ChefAction = helpers.ChefAction.clone();

        return chakram.post(endpoint(), ChefAction.json()).then(function(response) {
          expect(response).to.have.status(200)
          return helpers.ActionRefresh().then(function(response) {
            return chakram.get(actionsUrl()).then(function(response) {
              expect(response).to.have.status(200);
              expect(response.body.hits.total).to.equal(1);
            });
          });
        });
      });
    });

    describe("Delete action by node_id", function () {
      before(function () { return helpers.ESCleanupDocuments() });

      it("should return 200 and set node-state exists to false", function () {
        this.timeout(4000);
        let entityUuid = 'test_id';
        let chefRun = helpers.ChefRun.clone();
        chefRun.set('entity_uuid', entityUuid);

        let chefAction = helpers.ChefAction.clone();
        chefAction.set('node_id', entityUuid);
        chefAction.set('task', 'delete');

        // Wait for the delete action request to complete
        return chakram.post(chefRunEndpoint(), chefRun.json()).then(function(response) {
          expect(response).to.have.status(200);
          // Wait for a refresh of the node state index
          return helpers.NodeStateRefresh().then(function(response) {
            // Get all the node state index docs from elasticsearch
            return chakram.get(nodeStateUrl()).then(function(response) {
              expect(response).to.have.status(200)
              expect(response.body.hits.total).to.equal(1);
              let source = response.body.hits.hits[0]._source
              expect(source.entity_uuid).to.equal(entityUuid);
              let originalTimestamp = source.timestamp;
              expect(source.exists).to.equal(true);
              return chakram.post(endpoint(), chefAction.json()).then(function(response) {
                expect(response).to.have.status(200)
                return helpers.NodeStateRefresh().then(function(response) {
                  return chakram.get(nodeStateUrl()).then(function(response) {
                    expect(response).to.have.status(200)
                    expect(response.body.hits.total).to.equal(1);
                    let source = response.body.hits.hits[0]._source
                    expect(source.entity_uuid).to.equal(entityUuid);
                    expect(source.exists).to.equal(false);

                    //ensure the timestamp was updated
                    expect(source.timestamp.localeCompare(originalTimestamp) != 0).to.equal(true);
                  });
                });
              });
            });
          });
        });
      });

      after(function () { return helpers.ESCleanupDocuments() });
    });

    describe("Delete action by fields", function () {
      before(function () { return helpers.ESCleanupDocuments() });
      it("should return 200 and set node-state exists to false", function () {
        this.timeout(4000);
        let entityUuid = 'test_id';
        let organizationName = 'organization_name';
        let serviceHostname = 'service_hostname';
        let nodeName = 'node_name';

        let chefRun = helpers.ChefRun.clone();
        chefRun.set('entity_uuid', entityUuid);
        chefRun.set('chef_server_fqdn', serviceHostname);
        chefRun.set('organization_name', organizationName);
        chefRun.set('node_name', nodeName);

        let chefAction = helpers.ChefAction.clone();
        chefAction.set('id', 'id');
        chefAction.set('node_id', '');
        chefAction.set('organization_name', organizationName);
        chefAction.set('service_hostname', serviceHostname);
        chefAction.set('entity_name', nodeName);
        chefAction.set('task', 'delete');

        // Wait for the post requests.
        return chakram.post(chefRunEndpoint(), chefRun.json()).then(function(response) {
          expect(response).to.have.status(200);
          // Wait for the refresh requests.
          return helpers.NodeStateRefresh().then(function(response) {
            // Wait for the get all docs request
            return chakram.get(nodeStateUrl()).then(function(response) {
              expect(response).to.have.status(200)
              expect(response.body.hits.total).to.equal(1);
              let source = response.body.hits.hits[0]._source
              expect(source.entity_uuid).to.equal(entityUuid);
              expect(source.exists).to.equal(true);
              return chakram.post(endpoint(), chefAction.json()).then(function(response) {
                expect(response).to.have.status(200)
                return helpers.NodeStateRefresh().then(function(response) {
                  return chakram.get(nodeStateUrl()).then(function(response) {
                    expect(response).to.have.status(200)
                    expect(response.body.hits.total).to.equal(1);
                    let source = response.body.hits.hits[0]._source
                    expect(source.entity_uuid).to.equal(entityUuid);
                    expect(source.exists).to.equal(false);
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
});
