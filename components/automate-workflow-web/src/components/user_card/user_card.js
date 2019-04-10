import ng from 'angular';
import Users from '../../common/users/users';
import Flash from '../../common/ui/flash/flash';
import ApiUrl from '../../common/api/api_url';
import userCardTemplate from './user_card.html';
import Modal from '../../common/ui/modal/modal';
import confirmUserTypeChangeModal
  from '../../common/ui/modal/confirm_user_type_change_modal.html';

userCardComponent.$inject = ['$http', 'Users', 'Flash', 'ApiUrl', 'Modal'];

function userCardComponent($http, Users, Flash, ApiUrl, Modal) {

  function link(scope, element, attrs) {
    var username = scope.user.name;
    scope.currentUserType = 'internal';
    scope.showEditForm = false;
    scope.externalDisabled = false;
    scope.samlDisabled = false;
    scope.internalDisabled = false;

    scope.toggleEditForm = () => {
      if(!scope.showEditForm){
        scope.user.$fetch().$promise.then(fetchSuccess, fetchFailure);
      } else {
        scope.showEditForm = false;
        scope.user.password = null;
      }
    };

    function fetchFailure(response) {
      Flash.error('Permission denied', 'You must be an Admin to edit users.');
    }

    function fetchSuccess(user) {
      scope.showEditForm = true;
      scope.initializeAuthType(user.userType);
      scope.user.roles = null;
      scope.user.password = null;

      $http.get(ApiUrl('/authz/users/' + scope.user.name))
        .then((resp) => {
          scope.user.roles = Object.keys(resp.data);
        }, (resp) => {
          if(resp.status === 403){
            scope.externalDisabled = true;
            scope.samlDisabled = true;
            scope.internalDisabled = true;
          }
        });
    }

    scope.initializeAuthType = (userType) => {
      scope.currentUserType = userType;
      if(userType == 'external'){
        scope.internalDisabled = true;
      }
      if(userType == 'saml'){
        scope.internalDisabled = true;
      }
    };

    scope.cancel = () => {
      scope.showEditForm = false;
    };

    scope.switchUserType = (userType) => {
      scope.user.userType = userType;
    };

    scope.saveAndClose = () => {
      if(scope.currentUserType != scope.user.userType){
        scope.openModal();
      } else {
        scope.confirmSave();
      }
    };

    scope.confirmSave = () => {
      Users.update(username, scope.user).then(onSuccess, onFail);
    };

    function onSuccess() {
      username = scope.user.name;
      Flash.notify("Success", "You have successfully updated " + username + ".");
      scope.showEditForm = false;
    }

    function onFail(response) {
      Flash.error("Error Updating user", response.data.message);
      if (!response.userFailed) { // see if the username could have been updated
        username = scope.user.name;
      }
    }

    scope.openModal = () => {
      Modal.open(
        'Confirmation',
        confirmUserTypeChangeModal,
        'red-modal',
        scope
      );
    };

    scope.closeModal = () => {
      Modal.close();
    };

    scope.getPrettyText = (userType) => {
      if(userType == 'external'){
        return "an LDAP";
      } else if(userType == 'saml'){
        return "a SAML";
      } else if(userType == 'internal'){
        return "a Delivery";
      }
    };
  }

  return {
    template: userCardTemplate,
    scope: {
      user: '='
    },
    link: link
  };
}

export default ng
  .module('cd.components.userCard', [
    Users,
    Flash,
    ApiUrl,
    Modal
  ])
  .directive('cdUserCard', userCardComponent)
  .name;
