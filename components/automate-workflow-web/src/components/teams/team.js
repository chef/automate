import ng from 'angular';
import teamTemplate from './team.html';

teamComponent.$inject = ['$http', 'Flash', 'ApiUrl'];

function teamComponent($http, Flash, ApiUrl) {
  function link(scope) {
    function submit() {
      let user = scope.usernameEntry;
      let team = scope.team.name;
      let resource = '/teams/'+team+'/members/'+user;
      $http.put(ApiUrl(resource), {}, { ignoreApiInterceptor: true })
        .then(() => {
          scope.usernameEntry = '';
        }, () => {
          Flash.error('Error adding user', `There was a problem adding user ${user} to the team ${team}. Please try again.`);
        })
        .finally(() => {
          updateMembers();
        });
    }
    scope.submit = submit;

    function updateMembers() {

      $http.get(ApiUrl(`/teams/${scope.team.name}/members`))
        .then((resp) => {
          scope.members = resp.data.members;
        })
        .catch(() => {
          scope.members = [];
        });
    }
  }

  return {
    link: link,
    template: teamTemplate
  };
}

export default ng
  .module('cd.routes.teams.team', [])
  .directive('cdTeam', teamComponent)
  .name;
