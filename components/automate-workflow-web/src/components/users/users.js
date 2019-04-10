import ng from 'angular';
import usersTemplate from './users.html';
import Flash from '../../common/ui/flash/flash';
import Users from '../../common/users/users';
import userCard from '../user_card/user_card';

usersComponent.$inject = ['Flash', '$location', '$anchorScroll', 'Users'];

function usersComponent(Flash, $location, $anchorScroll, Users) {

  function link (scope) {

    scope.showNewUserForm = false;

    scope.userType = 'internal';

    scope.selectUserType = function (type) {
      scope.userType = type;
    };

    scope.toggleNewUserForm = function() {
      scope.showNewUserForm = !scope.showNewUserForm;
      scope.user = { roles: [] };
    };

    scope.submit = function (user) {
      switch (scope.userType) {
        case 'internal':
          Users.createInternal(user)
            .then(scope.onSuccess)
            .catch(scope.onFail);
          break;
        case 'external':
          Users.createExternal(user)
            .then(scope.onSuccess)
            .catch(scope.onFail);
          break;
        case 'saml':
          Users.createSaml(user)
            .then(scope.onSuccess)
            .catch(scope.onFail);
          break;
      }
    };

    scope.cancel = function () {
      scope.showNewUserForm = false;
    };

    scope.onSuccess = (user) => {
      scope.showNewUserForm = false;
      scope.users.$refresh().$promise.then(function() {
        $location.hash(user.name);
        $anchorScroll();
      });
      Flash.notify('Success', 'You have successfully added a new user.');
    };

    scope.onFail = (response) => {
      Flash.error('Error', 'There was an error saving your new user.');
    };
  }
  return {
    link: link,
    template: usersTemplate,
    scope: {
      users: '=users'
    }
  };
}

export default ng
  .module('cd.routes.users.component', [
    Flash,
    userCard,
    Users
  ])
  .directive('cdUsers', usersComponent)
  .name;
