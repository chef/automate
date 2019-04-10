import ng from 'angular';
import uiRouter from 'angular-ui-router';
import Auth from './auth_service';

authInterceptor.$inject = ['$q', '$injector'];

// This interceptor detects a 401 (Unauthorized) server response from
// _any_ $http request in the application. When one happens, it clears
// the in-memory session and user data (via the Auth service) and
// redirects the user to the login screen.
//
// This allows us to gracefully handle situations such as the user's
// token having expired.
//
// Individual $http requests can bypass this handling by passing a truthy
// value in the skipRedirectOn401 property of the request config object.
// Currently, only the Auth service itself needs to do this, to protect
// against vicious cycles of auth requests to the server when e.g.
// explicitly signing out (thus revoking the token).
function authInterceptor($q, $injector) {
  return {
    responseError: function (resp) {
      if (resp.status === 401 && !resp.config.skipRedirectOn401) {
        // To avoid circular dependency errors ...annoying.
        var Auth = $injector.get('Auth');

        // Unauthenticate the user
        Auth.unAuthenticate();
      }

      return $q.reject(resp);
    }
  };
}

authConfig.$inject = ['$httpProvider'];

function authConfig($httpProvider) {
  $httpProvider.interceptors.push('authInterceptor');
}

export default ng
  .module('cd.common.auth.interceptor', [
    uiRouter,
    Auth
  ])
  .factory('authInterceptor', authInterceptor)
  .config(authConfig)
  .name;
