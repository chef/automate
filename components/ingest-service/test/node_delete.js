var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var helpers = require('./helpers.js');

// An endpoint just for sending delete messages, possibly to be used on a CLI
// distinct from the /events/chef/action endpoint, but should use the same pipeline
function endpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/nodedelete';
}

function nodeStateUrl() {
  return helpers.ELASTICSEARCH_URI + '/node-state/_search';
}

function chefRunEndpoint() {
  return helpers.REST_SERVICE_URI + '/events/chef/run';
}

describe("/events/chef/nodedelete", function () {
  describe("posting unsupported message", function () {
    it("should return 400 with empty json", function () {
      var response = chakram.post(endpoint(), {"username":"xyz","password":"xyz"});
      return chakram.waitFor([
        expect(response).to.have.status(400)
      ]);
    });
  });

  describe("posting example message no elastic search data", function () {
    // Because as soon as a Node is created it can be deleted, even if it's never been run, there's
    // a possibility automate might never know about it, and therefore we need it to not throw an
    // error if it has no record of the node we're trying to delete.
    it("should return 200", function () {
      let chefDelete = helpers.ChefDelete.clone();

      // Wait for the post and the refresh requests.
      return chakram.all([chakram.post(endpoint(), chefDelete.json())]).then(function(responses) {
        expect(responses[0]).to.have.status(200);
      });
    });
  });

  describe("posting example message with elastic search data", function () {
    describe("Delete action by node_id", function () {
      before(function () { return helpers.ESCleanupDocuments() });

      it("should return 200 and set node-state exists to false", function () {
        this.timeout(4000);
        let entityUuid = 'test_id';
        let chefRun = helpers.ChefRun.clone();
        chefRun.set('entity_uuid', entityUuid);

        let chefDelete = helpers.ChefDelete.clone();
        chefDelete.set('node_id', entityUuid);

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

              return chakram.post(endpoint(), chefDelete.json()).then(function(response) {
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

        let chefDelete = helpers.ChefDelete.clone();
        chefDelete.set('id', 'id');
        chefDelete.set('node_id', '');
        chefDelete.set('organization_name', organizationName);
        chefDelete.set('service_hostname', serviceHostname);
        chefDelete.set('node_name', nodeName);

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
              return chakram.post(endpoint(), chefDelete.json()).then(function(response) {
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

        it.only("should return 200 and set node-state exists to false for multiple nodes", function () {
          this.timeout(4000);
          let entityUuid = 'test_id';
          let entityUuid2 = 'second_test_id';
          let organizationName = 'organization_name';
          let serviceHostname = 'service_hostname';
          let nodeName = 'node_name';

          let chefRun = helpers.ChefRun.clone();
          chefRun.set('entity_uuid', entityUuid);
          chefRun.set('chef_server_fqdn', serviceHostname);
          chefRun.set('organization_name', organizationName);
          chefRun.set('node_name', nodeName);

          let chefRun2 = helpers.ChefRun.clone();
          chefRun2.set('entity_uuid', entityUuid2);
          chefRun2.set('chef_server_fqdn', serviceHostname);
          chefRun2.set('organization_name', organizationName);
          chefRun2.set('node_name', nodeName);

          let chefDelete = helpers.ChefDelete.clone();
          chefDelete.set('id', 'id');
          chefDelete.set('node_id', '');
          chefDelete.set('organization_name', organizationName);
          chefDelete.set('service_hostname', serviceHostname);
          chefDelete.set('node_name', nodeName);

          // Wait for the post requests.
          return chakram.post(chefRunEndpoint(), chefRun.json()).then(function(response) {
            expect(response).to.have.status(200);
            return chakram.post(chefRunEndpoint(), chefRun2.json()).then(function(response) {
              expect(response).to.have.status(200);
            // Wait for the refresh requests.
            return helpers.NodeStateRefresh().then(function(response) {
              // Wait for the get all docs request
              return chakram.get(nodeStateUrl()).then(function(response) {
                expect(response).to.have.status(200)
                expect(response.body.hits.total).to.equal(2);
                let source = response.body.hits.hits[0]._source
                expect(source.exists).to.equal(true);
                let source2 = response.body.hits.hits[1]._source
                expect(source2.exists).to.equal(true);
                return chakram.post(endpoint(), chefDelete.json()).then(function(response) {
                  expect(response).to.have.status(200)
                  return helpers.NodeStateRefresh().then(function(response) {
                    return chakram.get(nodeStateUrl()).then(function(response) {
                      expect(response).to.have.status(200)
                      expect(response.body.hits.total).to.equal(2);
                      let source = response.body.hits.hits[0]._source
                      expect(source.exists).to.equal(false);
                      let source2 = response.body.hits.hits[1]._source
                      expect(source2.exists).to.equal(false);
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
});
