import ng from 'angular';
import uiRouter from 'angular-ui-router';
import AuthService from './auth_service';

authInitializer.$inject = ['$rootScope', 'Auth', '$state'];

function authInitializer($rootScope, Auth, $state) {
  $rootScope.$on('$stateChangeStart', (event, toState, toParams, fromState, fromParams) => {
    Auth.isAuthenticated().catch(() => {
      Auth.unAuthenticate();
    });
  });
}

export default ng
  .module('cd.common.auth.initializer', [
    uiRouter,
    AuthService
  ])
  .run(authInitializer)
  .name;
