import ng from 'angular';
import ApiUrl from '../../common/api/api_url';
import Flash from '../../common/ui/flash/flash';
import Modal from '../../common/ui/modal/modal';
import Session from '../../common/auth/session';
import smtpSetupTemplate from './smtp_setup.html';
import 'angular-spinner/angular-spinner';
import removeSmtpConfirmationModal
  from '../../common/ui/modal/remove_smtp_confirmation_modal.html';

smtpSetupComponent.$inject = ['$http', 'ApiUrl', 'Flash', 'Modal', 'Session'];

function smtpSetupComponent($http, ApiUrl, Flash, Modal, Session) {

  function link(scope) {
    let dummyPassword = "**********";
    let passwordChange = false;

    scope.enterprise = Session.get().enterprise;
    scope.authorized = scope.smtp.authorized;
    scope.config = scope.smtp.config || {};
    scope.configExists = !!scope.smtp.config;

    scope.userEmail = () => {
      return scope.smtp.user.email;
    };

    if(scope.configExists) {
      scope.config.password = dummyPassword;
    }

    scope.save = () => {
      let config = angular.copy(scope.config);
      if(!passwordChange) {
          delete config.password;
      }
      $http.put(ApiUrl('/notifications/smtp'), config)
        .then((resp) => {
          Flash.notify('Success', 'Your SMTP email setup has been saved.');
          scope.config.password = dummyPassword;
          passwordChange = false;
        })
        .catch(() => {
          Flash.error('Error', 'Could not save. Please fill out all required fields.');
        });
    };

    scope.editPassword = () => {
      passwordChange = true;
    };

    scope.testSmtp = function () {
      scope.testResult = '';
      scope.testRunning = true;
      let config = angular.copy(scope.config);
      if(!passwordChange) {
        delete config.password;
        scope.config.password = dummyPassword;
      }
      $http.post(ApiUrl('/notifications/smtp/test'), config)
        .then(() => {
          scope.testRunning = false;
          scope.testResult = 'success';
        })
        .catch(() => {
          scope.testRunning = false;
          scope.testResult = 'error';
        });
    };

    scope.confirmDelete = () => {
      Modal.open(
        'Confirmation',
        removeSmtpConfirmationModal,
        'red-modal',
        scope
      );
    };

    scope.deleteSmtp = () => {
      scope.closeModal();

      $http.delete(ApiUrl('/notifications/smtp'))
        .then((resp) => {
          Flash.notify('Success', 'Your SMTP email setup has been deleted.');
          scope.config = {};
          scope.configExists = false;
        })
        .catch(() => {
          Flash.error('Error', 'Could not delete saved configuration.');
        });
    };

    scope.closeModal = () => {
      Modal.close();
    };
  }

  return {
    link: link,
    template: smtpSetupTemplate,
    scope: {
      smtp: '='
    }
  };
}

export default ng
  .module('cd.routes.smtp_setup', [
    ApiUrl,
    'angularSpinner',
    Flash,
    Modal,
    Session
  ])
  .directive('cdSmtpSetup', smtpSetupComponent)
  .name;
