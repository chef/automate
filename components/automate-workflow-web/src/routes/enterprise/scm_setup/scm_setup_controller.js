import ng from 'angular';
import { omit } from 'lodash';
import ApiUrl from '../../../common/api/api_url';
import Flash from '../../../common/ui/flash/flash';
import Modal from '../../../common/ui/modal/modal';
import removeScmConfirmationModal
  from  '../../../common/ui/modal/remove_scm_confirmation_modal.html';

scmSetupController.$inject = [
  '$http',
  '$scope',
  'Flash',
  'Modal',
  'scmProviders',
  'scmSetup',
  'authorized'
];

function scmSetupController($http, $scope, Flash, Modal, scmProviders, scmSetup, authorized) {
  let dummyPassword = "********";
  
  $scope.authorized = authorized;
  $scope.scmSetup = scmSetup;

  if ($scope.scmSetup.bitbucket) {
    $scope.scmSetup.bitbucket.password = dummyPassword;
  }
  if ($scope.scmSetup.github) {
    $scope.scmSetup.github.password = dummyPassword;
  }
  $scope.scmProviders = scmProviders;

  $scope.selectedProvider = scmProviders[0];

  $scope.selectProvider = (provider) => {
    $scope.selectedProvider = provider;
  };

  $scope.editBitbucketPassword = () => {
    $scope.scmSetup.bitbucket.passwordChange = true;
  };

  $scope.editGithubPassword = () => {
    $scope.scmSetup.github.passwordChange = true;
  };

  $scope.save = () => {
    if ($scope.scmSetupForm.$valid) {
      let { type, setupUrl } = $scope.selectedProvider;
      let setupData = omit($scope.scmSetup[type], '_links', 'passwordChange');
      let formattedType = $scope.formattedType(type);

      $http
        .post(setupUrl, setupData, {'ignoreApiInterceptor':true})
        .then((resp) => {
          Flash.notify('Success', 'You have set up your ' + formattedType + ' SCM link successfully.');
          if (resp.status == 201) {
            $http
              .get(setupUrl)
              .then((resp) => {
                $scope.scmSetup[type] = resp.data[0] || {};
                $scope.scmSetup[type].password = "********";
                $scope.scmSetup[type].passwordChange = false;
              });
          }
        })
        .catch((resp) => {
          if (resp.data.message) {
            Flash.error('Error', resp.data.message);
          } else {
            Flash.error('Error', 'Sorry, something went wrong.');
          }
        });
     }
  };

  $scope.update = () => {
    if ($scope.scmSetupForm.$valid) {
      let { type, setupUrl } = $scope.selectedProvider;
      let updateUrl = $scope.scmSetup[type]._links.self.href;
      let setupData = omit($scope.scmSetup[type], '_links', 'passwordChange');
      let formattedType = $scope.formattedType(type);
      // Do not send the password to the backend if there is no change in the password field.
      // Since there are two scm types, we need to ensure that the password
      // edited is for the corresponding type.
      if (!$scope.scmSetup[type].passwordChange) {
        setupData = omit(setupData, 'password');
      }

      $http
        .put(updateUrl, setupData, {'ignoreApiInterceptor':true})
        .then((resp) => {
          Flash.notify('Success', 'You have updated your ' + formattedType + ' SCM successfully.');
          $scope.scmSetup[type] = resp.data || {};
          $scope.scmSetup[type].password = dummyPassword;
          $scope.scmSetup[type].passwordChange = false;
        })
        .catch((resp) => {
          Flash.error('Error', resp.data.message);

        });
     }
  };

  $scope.delete = () => {
    Modal.open(
      'Confirmation',
      removeScmConfirmationModal,
      'red-modal',
      $scope
    );
  };

  $scope.doDelete = (scmSetup) => {
    Modal.close();
    let { type, setupUrl } = $scope.selectedProvider;
    let deleteUrl = scmSetup[type]._links.self.href;
    let formattedType = $scope.formattedType(type);

    $http
      .delete(deleteUrl, {'ignoreApiInterceptor':true})
      .then((resp) => {
        Flash.notify('Success', 'You have removed the ' + formattedType + ' SCM link successfully.');
        $scope.scmSetup[type] = {};
      })
      .catch((resp) => {
        Flash.error('Error', resp.data.message);
      });
  };

  $scope.closeModal = () => {
    Modal.close();
  };

  $scope.formattedType = (type) => {
    return type === "bitbucket" ? "Bitbucket" : "GitHub";
  };
}

export default ng
  .module('cd.routes.enterprise.scm_setup.controller', [
    ApiUrl,
    Flash,
    Modal
  ])
  .controller('scmSetupController', scmSetupController)
  .name;
