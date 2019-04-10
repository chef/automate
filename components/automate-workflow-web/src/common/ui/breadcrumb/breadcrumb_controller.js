import ng from 'angular';
import Breadcrumb from './breadcrumb_service';

breadcrumbController.$inject = ['$rootScope', '$scope', 'Breadcrumb'];

function breadcrumbController($rootScope, $scope, Breadcrumb) {
  $scope.breadcrumb = Breadcrumb.getCrumb();

  $rootScope.$on('updateBreadcrumb', function() {
    $scope.breadcrumb = Breadcrumb.getCrumb();
  });

  $scope.trigger = function (funstring, args) {
    $rootScope.$broadcast(funstring);
  };
}

export default ng
  .module('cd.common.ui.breadcrumb.controller', [
    Breadcrumb
  ])
  .controller('breadcrumbController', breadcrumbController)
  .name;
