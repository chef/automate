import ng from 'angular';
import { pick } from 'lodash';
import User from '../models/user';
import ApiUrl from '../api/api_url';

Users.$inject = ['$http', '$q', 'User', 'ApiUrl'];

function Users($http, $q, User, ApiUrl) {

  function createUserOnEndpoint(endpoint) {
    return function (user) {

      function createUser() {
        return $http.post(ApiUrl(endpoint), pick(user, ['name', 'ssh_pub_key']));
      }

      function setRoles() {
        return $http.post(ApiUrl('/authz/users/' + user.name), { set: user.roles });
      }

      return createUser()
        .then(setRoles)
        .then(function () {
          return User.$find(user.name).$promise;
        });
    };
  }

  return {
    createInternal: function (user) {

      function createUser() {
        return $http.post(ApiUrl('/internal-users'), pick(user, ['first', 'last', 'email', 'name', 'ssh_pub_key']));
      }

      function setPassword() {
        return $http.post(ApiUrl('/internal-users/' + user.name + '/change-password'), { password: user.password });
      }

      function setRoles() {
        return $http.post(ApiUrl('/authz/users/' + user.name), { set: user.roles });
      }

      return createUser()
        .then(setPassword)
        .then(setRoles)
        .then(function () {
          return User.$find(user.name).$promise;
        });
    },

    createExternal: createUserOnEndpoint('/external-users'),

    createSaml: createUserOnEndpoint('/saml-users'),

    update: function (name, user) {

      user.ssh_pub_key = user.sshPubKey || '';
      user.user_type = user.userType;

      function updateUser() {
        return $http.put(ApiUrl('/users/' + name), pick(user, ['first', 'last', 'email', 'name', 'ssh_pub_key', 'user_type']))
          // tell the error handler that the username hasn't changed
          .catch(function (response) {
            return $q.reject({
              userFailed: true,
              data: response.data });
          });
      }

      function setPassword() {
        return $http.post(ApiUrl('/internal-users/' + user.name + '/change-password'), { password: user.password });
      }

      function setRoles() {
        return $http.post(ApiUrl('/authz/users/' + user.name), { set: user.roles });
      }

      return updateUser()
        .then(function () { if (user.links.changePassword && user.password) { return setPassword(); } })
        .then(function () { if (user.roles) { return setRoles(); } })
        .then(function () {
          return User.$find(user.name).$promise;
        });
    }
  };
}

export default ng
  .module('common.users', [
    ApiUrl,
    User
  ])
  .service('Users', Users)
  .name;
