import ng from 'angular';
import Flash from '../../common/ui/flash/flash';
import teamsTemplate from './teams.html';
import ApiUrl from '../../common/api/api_url';
import TeamsService from '../../common/teams/teams_service';

teamsComponent.$inject = ['$http', '$location','Flash', 'ApiUrl', 'TeamsService'];

function teamsComponent($http, $location, Flash, ApiUrl, TeamsService) {

  function link(scope) {
    scope.showNewTeamForm = false;
    scope.team = {};

    scope.toggleNewTeamForm = function() {
      scope.showNewTeamForm = !scope.showNewTeamForm;
      scope.team = {};
    };

    scope.cancel = () => {
      scope.showNewTeamForm = false;
    };

    scope.submit = () => {

      TeamsService.create(scope.team)
        .then(() => {
          $location.url('/teams/' + scope.team.name);
        })
        .catch(() => {
          Flash.error('Error adding team', 'There was a problem adding your team. Please try again.');
        });
    };
  }

  return {
    link: link,
    template: teamsTemplate
  };
}

export default ng
  .module('cd.routes.teams', [
    ApiUrl,
    Flash,
    TeamsService
  ])
  .directive('cdTeams', teamsComponent)
  .name;
