import ng from 'angular';

teamController.$inject = ['$scope', 'team', 'members', 'users', 'isAdmin'];


function teamController($scope, team, members, users, isAdmin) {
  $scope.team = team;
  $scope.members = members;
  $scope.users = users;
  $scope.usernameInvalid = true;
  $scope.isAdmin = isAdmin;

  $scope.filterUsers = (element) => {
     let entry = $scope.usernameEntry;
     return element.startsWith(entry);
  };

  $scope.validateUserField = () => {
     return users.indexOf($scope.usernameEntry) >= 0;
  };

}

export default ng
  .module('cd.routes.enterprise.team.controller', [])
  .controller('teamController', teamController)
  .name;
