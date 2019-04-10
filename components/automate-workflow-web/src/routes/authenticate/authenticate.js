import ng from 'angular';
import uiRouter from 'angular-ui-router';
import authenticateController from './authenticate_controller';
import authenticateComponent from './authenticate_component';
import welcome from './welcome/welcome';
import login from './login/login';

export default ng
  .module('cd.routes.authenticate', [
    uiRouter,
    authenticateController,
    authenticateComponent,
    welcome,
    login
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('authenticate', {
        template: '<cd-authenticate>',
        controller: 'authenticateController'
      });
  })
  .config(($stateProvider) => {
    $stateProvider
      .state('reset_password', {
        url: '/reset-password/:username/:token',
        template: '<reset-password [token]="token" [username]="username">',
        controller: ($rootScope, $scope, $stateParams) => {
          $rootScope.app_state = 'login';
          $scope.username = $stateParams.username;
          $scope.token = $stateParams.token;
        }
      });
  })
  .name;
