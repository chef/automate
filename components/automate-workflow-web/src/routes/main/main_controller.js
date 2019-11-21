import ng from 'angular';
import uiRouter from 'angular-ui-router';
import Store from '../../common/store/store';

mainController.$inject = [
  '$rootScope',
  '$scope',
  'Store'
];

function mainController(
  $rootScope, $scope, Store) {
  $rootScope.app_state = "main";
  $rootScope.session = Store.get('session');
  $rootScope.enterpriseHref = function (enterprise) {
    return "/workflow/e/" + enterprise + "/"
  };

  $scope.nav_state = Store.get('nav_state') || "";
}

export default ng
  .module('cd.routes.main.controller', [
    uiRouter,
    Store
  ])
  .controller('mainController', mainController)
  .name;
