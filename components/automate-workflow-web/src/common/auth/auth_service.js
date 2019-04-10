
import ng from 'angular';
import uiRouter from 'angular-ui-router';
import ApiUrl from '../api/api_url';
import Session from './session';
import CurrentUser from './current_user';
import appConfig from '../../config';

Auth.$inject = ['$http', 'ApiUrl', 'Session', 'CurrentUser', '$q', 'appConfig', '$window'];

function Auth($http, ApiUrl, Session, CurrentUser, $q, appConfig, $window) {

  function auth(username, token) {
    Session.set({ username, token });
  }

  function unauth() {

    if (Session.hasSession()) {
      Session.remove();
      CurrentUser.removeUser();
      localStorage.removeItem('canonical-enterprise');
    }

    $window.location.assign('/');
  }

  return {
    isAuthenticated: () => {
      return $q((resolve, reject) => {

        if (Session.hasSession()) {
          const { username, token } = Session.get();

          $http.head(ApiUrl('/verify-token'), { skipRedirectOn401: true })
            .then(() => {
              auth(username, token);
              resolve('Authorized');
            })
            .catch((_, status) => {
              reject('Rejected with status code' + status);
            });
        } else {
          reject('no session to verify');
        }
      });
    },

    unAuthenticate: () => {
      unauth();
    },

    authenticate: (username, token) => {
      auth(username, token);
    }
  };
}

export default ng
  .module('cd.common.auth.service', [
    uiRouter,
    ApiUrl,
    Session
  ])
  .service('Auth', Auth)
  .name;
