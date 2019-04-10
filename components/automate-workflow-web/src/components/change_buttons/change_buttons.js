import ng from 'angular';
import uiRouter from 'angular-ui-router';
import Modal from '../../common/ui/modal/modal';
import Flash from '../../common/ui/flash/flash';
import changeButtonsTemplate from './change_buttons.html';
import approveConfirmationModalTemplate
  from '../../common/ui/modal/approve_confirmation_modal.html';
import deleteConfirmationModalTemplate
  from  '../../common/ui/modal/delete_confirmation_modal.html';
import deliverConfirmationModalTemplate
  from  '../../common/ui/modal/deliver_confirmation_modal.html';

changeButtonsDirective.$inject = ['Modal', 'Flash', '$http', '$state'];

function changeButtonsDirective(Modal, Flash, $http, $state) {

  function link(scope, element, attrs) {
    scope.delete = function () {
      Modal.open(
        'Confirmation',
        deleteConfirmationModalTemplate,
        'red-modal',
        scope
      );
    };

    scope.doDelete = function (change) {
      Modal.close();
      change.delete()
        .then(function (resp) {
          $state.go('main.enterprise.organizations.organization.project');
        })
        .catch(function (resp) {
          Flash.error('Error', 'There was an error. Your change may have not been deleted successfully.');
        });
    };

    scope.closeModal = function () {
      Modal.close();
    };

    scope.doApprove = function (change) {
      Modal.close();
      change.approve()
        .then(function (resp) {
          $state.go('main.enterprise.organizations.organization.project.change.status');
        })
        .catch(function (resp) {
          if (resp.data && resp.data.message) {
            Flash.error('Error', resp.data.message);
          } else {
            Flash.error('Error', 'Change failed to merge.');
          }
        });
    };

    scope.approve = function (change) {
      Modal.open(
        'Confirmation',
        approveConfirmationModalTemplate,
        'info-modal',
        scope
      );
    };

    scope.deliver = function (change) {
      var modalScope = scope.$new();
      var changelogUrl = scope.project.pipelines.$url() +
        '/' + change.target + '/changelog/unshipped';

      $http.get(changelogUrl).then(function (resp) {
        modalScope.changelog = resp.data;
      });

      Modal.open(
        'Confirm Delivery',
        deliverConfirmationModalTemplate,
        'info-modal',
        modalScope
      );
    };

    scope.doDeliver = function (change) {
      Modal.close();
      change.deliver()
        .catch(function (resp) {
          Flash.error('Error', 'There was an error. Your change may have not been delivered successfully.');
        });
    };
  }

  return {
    template: changeButtonsTemplate,
    link: link
  };
}

export default ng
  .module('cd.components.changeButtons', [
    uiRouter,
    Modal,
    Flash
  ])
  .directive('cdChangeButtons', changeButtonsDirective)
  .name;
