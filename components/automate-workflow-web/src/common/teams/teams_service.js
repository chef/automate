import ng from 'angular';

TeamsService.$inject = ['$http', 'ApiUrl'];

function TeamsService($http, ApiUrl) {

  return {
    teams: [],

    fetch: function() {
      return $http.get(ApiUrl('/teams'))
        .then((resp) => {
          this.teams = resp.data.teams;
        });
    },

    create: function(params) {
      return $http.post(ApiUrl('/teams'), params, { ignoreApiInterceptor: true })
        .then(() => {
          this.fetch();
        });
    }
  };
}

export default ng
  .module('common.teams', [])
  .factory('TeamsService', TeamsService)
  .name;
