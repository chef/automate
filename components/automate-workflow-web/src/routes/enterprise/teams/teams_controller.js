import ng from 'angular';

teamsController.$inject = ['$scope', 'teams', 'isAdmin'];

function teamsController($scope, teams, isAdmin) {
  $scope.teams = teams;
  $scope.isAdmin = isAdmin;
}

export default ng
  .module('cd.routes.enterprise.teams.controller', [])
  .controller('teamsController', teamsController)
  .name;
