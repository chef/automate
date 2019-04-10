import ng from 'angular';
import ApiUrl from '../api/api_url';
import User from '../models/user';
import Session from './session';

CurrentUser.$inject = ['$http', 'User', 'Session', 'ApiUrl'];

function CurrentUser($http, User, Session, ApiUrl) {
  let currentUser;

  return {
    user: () => {
      let userName = Session.get('username');

      if (!currentUser && userName) {
        currentUser = User.$find(userName);

        Object.defineProperty(currentUser, 'displayName', {
          get: () => {
            var first = currentUser.first;
            var last = currentUser.last;
            var display = currentUser.name;

            if (first && last) {
              display = currentUser.first + ' ' + currentUser.last;
            }

            return display;
          }
        });

        $http.get(ApiUrl('/authz/users/' + userName)).then(function (resp) {
          currentUser.roles = Object.keys(resp.data);
        });
      }

      return currentUser;
    },
    removeUser: () => {
      currentUser = null;
    }
  };
}

export default ng
  .module('cd.common.auth.currentUser', [
    User,
    Session,
    ApiUrl
  ])
  .service('CurrentUser', CurrentUser)
  .name;
