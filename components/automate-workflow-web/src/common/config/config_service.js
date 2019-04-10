import ng from 'angular';

ConfigService.$inject = ['$http', '$rootScope'];

function ConfigService($http, $rootScope) {
  var config = { a2Mode: null };
  return {
    setConfig: () => {
      return $http.get("/workflow/api/_status", { cache: true }).then(function(resp) {
        if (resp.data.a2_mode === "true") {
          //Should not matter once AIA-499 is merged
          $rootScope.nodesLink = window.location.protocol + "//" + window.location.hostname + "/workflow/nodes";
          $rootScope.complianceLink = window.location.protocol + "//" + window.location.hostname + "/workflow/compliance";
        } else {
          $rootScope.nodesLink = "/viz/#/";
          $rootScope.complianceLink = "/viz/#/compliance";
        }
      });
    }
  };
}

export default ng
  .module('common.config', [])
  .service('ConfigService', ConfigService)
  .name;
