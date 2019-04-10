import ng from 'angular';
import Session from '../../common/auth/session';

authenticateController.$inject = ['$rootScope', 'Session'];

function authenticateController($rootScope, Session) {
  $rootScope.app_state = 'login';
}

export default ng
  .module('cd.routes.authenticate.controller', [
    Session
  ])
  .controller('authenticateController', authenticateController)
  .name;
