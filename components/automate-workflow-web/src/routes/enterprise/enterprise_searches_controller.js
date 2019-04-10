import ng from 'angular';

enterpriseSearchesController.$inject = ['$scope', 'searches'];

function enterpriseSearchesController($scope, searches) {

  $scope.searches = searches;
}

export default ng
  .module('cd.routes.enterprise.searches.controller', [])
  .controller('enterpriseSearchesController', enterpriseSearchesController)
  .name;
