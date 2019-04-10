var request = require('request');
var chakram = require('chakram'),
  expect = chakram.expect;
var fs = require('fs');

// Global variables
var REST_SERVICE_URI = process.env.REST_SERVICE || 'http://0.0.0.0:2192';
var ELASTICSEARCH_URI = process.env.ELASTICSEARCH_URL || 'http://elasticsearch:9200';
var DEBUG = process.env.DEBUG || false;

var ChefRun = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefRun = Object.create(this);
    newChefRun.data = JSON.parse(fs.readFileSync('examples/converge-success-report.json', 'utf8'));
    return newChefRun;
  }
};

var ChefRunFailure = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefRun = Object.create(this);
    newChefRun.data = JSON.parse(fs.readFileSync('examples/converge-failure-report.json', 'utf8'));
    return newChefRun;
  }
};

var ChefRunBad = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefBadRun = Object.create(this);
    newChefBadRun.data = JSON.parse(fs.readFileSync('examples/converge-bad-report.json', 'utf8'));
    return newChefBadRun;
  }
};

var ChefAction = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefAction = Object.create(this);
    newChefAction.data = JSON.parse(fs.readFileSync('examples/chef_action.json', 'utf8'));
    return newChefAction;
  }
};

var ChefCCRAction = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefAction = Object.create(this);
    newChefAction.data = JSON.parse(fs.readFileSync('examples/chef_CCR_action.json', 'utf8'));
    return newChefAction;
  }
};

var ChefDelete = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefDelete = Object.create(this);
    newChefDelete.data = JSON.parse(fs.readFileSync('examples/chef_delete.json', 'utf8'));
    return newChefDelete;
  }
};

var ChefLiveness = {
  json: function () {
    return this.data;
  },
  set: function (key, value) {
    this.data[key] = value;
  },
  clone: function() {
    var newChefLiveness = Object.create(this);
    newChefLiveness.data = JSON.parse(fs.readFileSync('examples/liveness_ping.json', 'utf8'));
    return newChefLiveness;
  }
}

// Refreshes the converge-history index
var ConvergeHistoryRefresh = function (date) {
  return new Promise(function (resolve) {
    debug('Refreshing ES converge-history indexes');
    request.post({
      uri: ELASTICSEARCH_URI + '/converge-history-*/_refresh'
    }, function (error, response, body) {
      if (error) { displayError(error, response); };
      debug("---> INDEX=converge-history REFRESHED!");
      resolve();
    });
  });
}

// Refreshes the action index
var ActionRefresh = function (date) {
  return new Promise(function (resolve) {
    debug('Refreshing ES actions indexes');
    request.post({
      uri: ELASTICSEARCH_URI + '/actions-*/_refresh'
    }, function (error, response, body) {
      if (error) { displayError(error, response); };
      debug("---> INDEX=actions REFRESHED!");
      resolve();
    });
  });
}

// Refreshes the node-state index
var NodeStateRefresh = function () {
  return new Promise(function (resolve) {
    debug('Refreshing ES node-state index');
    request.post({
      uri: ELASTICSEARCH_URI + '/node-state/_refresh'
    }, function (error, response, body) {
      if (error) { displayError(error, response); };
      debug("---> INDEX=node-state REFRESHED!");
      resolve();
    });
  });
}

// Refreshes the node-attribute index
var NodeAttributeRefresh = function () {
  return new Promise(function (resolve) {
    debug('Refreshing ES node-attribute index');
    request.post({
      uri: ELASTICSEARCH_URI + '/node-attribute/_refresh'
    }, function (error, response, body) {
      if (error) { displayError(error, response); };
      debug("---> INDEX=node-attribute REFRESHED!");
      resolve();
    });
  });
}

var ESConvergeHistoryCleanupDocuments = function () {
  return new Promise(function (resolve) {
    request.get({
      uri: ELASTICSEARCH_URI + '/converge-history-*'
    }, function (error, response, body) {
      let indexes = JSON.parse(response.body);
      let promises = []
      for(var index in indexes){
        debug('Deleting docs from index ' + index);
        let promise = new Promise(function (resolve) {
          deleteAll = {
            uri: ELASTICSEARCH_URI + '/' + index + '/_delete_by_query',
            body: {"query":{"match_all":{}}},
            json: true
          }
          request.post(deleteAll, function (error, response, body) {
            if (error) { displayError(error, response) }
            resolve();
          });
        });

        promises.push(promise);
      }
      Promise.all(promises).then( function (result) {
        resolve();
      }, function(err) {
        console.info("err: " + err);
      });
    });
  });
};

var ESCleanupActions = function () {
  return new Promise(function (resolve) {
    request.get({
      uri: ELASTICSEARCH_URI + '/actions-*',
    }, function (error, response, body) {
      let indexes = JSON.parse(response.body);
      let promises = []
      for(var index in indexes){
        debug('Deleting docs from index ' + index);
        let promise = new Promise(function (resolve) {
          deleteAll = {
            uri: ELASTICSEARCH_URI + '/' + index + '/_delete_by_query',
            body: {"query":{"match_all":{}}},
            json: true
          }
          request.post(deleteAll, function (error, response, body) {
            if (error) { displayError(error, response) }
            resolve();
          });
        });

        promises.push(promise);
      }
      Promise.all(promises).then( function (result) {
        resolve();
      }, function(err) {
        console.info("err: " + err);
      });
    });
  });
};


var ESCleanupDocuments = function () {
  let stateCleanup = new Promise(function (resolve) {
    debug('Deleting all documents from Elasticsearch');
    // Adding `?refresh=true` to ES Query to wait for a refresh
    deleteAll = {
      uri: ELASTICSEARCH_URI + '/node-state/_delete_by_query',
      body: {"query":{"match_all":{}}},
      json: true,
    }
    return request.post(deleteAll, function (error, response, body) {
      if (error) { displayError(error, response) }
      debug('-> Documents deleted!');
      resolve();
    });
  });

  let convergeHistoryCleanup = ESConvergeHistoryCleanupDocuments();
  let actionsHistoryCleanup = ESCleanupActions();

  let attributeCleanup = new Promise(function (resolve) {
    debug('Deleting all documents from Elasticsearch');
    // Adding `?refresh=true` to ES Query to wait for a refresh
    deleteAll = {
      uri: ELASTICSEARCH_URI + '/node-attribute/_delete_by_query',
      body: {"query":{"match_all":{}}},
      json: true,
    }
    return request.post(deleteAll, function (error, response, body) {
      if (error) { displayError(error, response) }
      debug('-> Documents deleted!');
      resolve();
    });
  });

  return Promise.all([stateCleanup, attributeCleanup, convergeHistoryCleanup, actionsHistoryCleanup]).then(function(responses) {
    return Promise.all([ConvergeHistoryRefresh(), ActionRefresh(), NodeStateRefresh(), NodeAttributeRefresh()]);
  });
};

// Exporting functions so that other test files can use them:
//
// Example
// ```
// var helpers = require('./helpers.js');
// helpers.ESInitIndexes();
// ```
exports.REST_SERVICE_URI       = REST_SERVICE_URI;
exports.ChefRun                = ChefRun;
exports.ChefRunBad             = ChefRunBad;
exports.ChefAction             = ChefAction;
exports.ChefDelete             = ChefDelete;
exports.ChefRunFailure         = ChefRunFailure;
exports.ELASTICSEARCH_URI      = ELASTICSEARCH_URI;
exports.ConvergeHistoryRefresh = ConvergeHistoryRefresh;
exports.ActionRefresh          = ActionRefresh;
exports.NodeStateRefresh       = NodeStateRefresh;
exports.NodeAttributeRefresh   = NodeAttributeRefresh;
exports.ESCleanupDocuments     = ESCleanupDocuments;
exports.ChefCCRAction          = ChefCCRAction;
exports.ChefLiveness           = ChefLiveness

// Private helper functions

function displayError(error, response) {
      console.log('error:', error);
      console.log('statusCode:', response && response.statusCode);
};

function debug(msg) {
  if (DEBUG) {
    console.log(msg);
  }
};
