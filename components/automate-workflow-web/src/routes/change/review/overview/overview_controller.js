import ng from 'angular';

overviewController.$inject = [
  '$scope',
  'organization',
  'project',
  'change'
];

function overviewController($scope, organization, project, change) {
  $scope.change = change;
}

export default ng
  .module('cd.routes.change.review.overview.controller', [])
  .controller('overviewController', overviewController)
  .name;
