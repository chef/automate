import ng from 'angular';
import Auth from '../../common/auth/auth';
import CurrentUser from '../../common/auth/current_user';
import userDropdownTemplate from './user_dropdown.html';
import licenseModalTemplate from '../../common/ui/modal/license_modal.html';
import Modal from '../../common/ui/modal/modal';
import Store from '../../common/store/store';

userDropdownComponent.$inject = [
  '$document',
  '$http',
  '$state',
  'Auth',
  'CurrentUser',
  'Modal',
  'Store'
];

function userDropdownComponent($document, $http, $state, Auth, CurrentUser, Modal, Store) {

  function link(scope, element) {
    scope.currentUser = CurrentUser.user();
    scope.dropdownShowing = false;

    scope.getBuildVersion = () => {
      $http.get('/workflow/status/version')
        .then((res) => {
          let text = res.data;
          let line = text.split('\n')[0];
          scope.buildVersion = line.match(/\d*\.\d*\.\d*/)[0];
        })
        .catch((error) => {
          scope.buildVersion = '0.0.1';
        });
    };

    scope.getBuildVersion();

    scope.toggleDropdown = () => {
      scope.dropdownShowing = !scope.dropdownShowing;
    };

    $document.on('click', (e) => {
      if (!element[0].contains(e.target)) {
        scope.$apply(scope.dropdownShowing = false);
      }
    });

    scope.logout = () => {
      Store.clear('session');
      Auth.unAuthenticate();
    };

    scope.openLicense = () => {
      Modal.open(
        'Chef Automate License Information',
        licenseModalTemplate,
        'default-modal',
        scope
      );
    };
  }

  return {
    link: link,
    template: userDropdownTemplate
  };
}

export default ng
  .module('cd.components.userDropdown', [
    Auth,
    CurrentUser,
    Modal,
    Store
  ])
  .directive('cdUserDropdown', userDropdownComponent)
  .name;
