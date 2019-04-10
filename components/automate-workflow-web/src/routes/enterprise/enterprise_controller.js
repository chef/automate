import ng from 'angular';

enterpriseController.$inject = ['$scope', 'orgs'];

function enterpriseController($scope, orgs) {
  $scope.orgs = orgs;
}

export default ng
  .module('cd.routes.enterprise.controller', [])
  .controller('enterpriseController', enterpriseController)
  .name;
