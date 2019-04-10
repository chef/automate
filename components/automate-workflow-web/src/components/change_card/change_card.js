import ng from 'angular';
import { every, first } from 'lodash';
import Flash from '../../common/ui/flash/flash';
import Modal from '../../common/ui/modal/modal';
import changeCardTemplate from './change_card.html';
import deleteConfirmationModalTemplate
  from  '../../common/ui/modal/delete_confirmation_modal.html';
import deliverConfirmationModalTemplate
  from  '../../common/ui/modal/deliver_confirmation_modal.html';
import moment from 'moment';

changeCardDirective.$inject = ['Flash', 'Modal'];

function changeCardDirective(Flash, Modal) {

  function link(scope, element, attrs) {

    // First patchset in change is the latest
    scope.patchset = first(scope.change.patchsets);

    scope.change.submitDate = moment(scope.change.submitAt).format('MMMM D, YYYY h:mm A');

    scope.toggleComments = function () {
      scope.showComments = !scope.showComments;
      if(scope.showComments) scope.change.comments.$refresh();
    };

    scope.$watch(function () {
      return every(scope.change.stages, {'status': 'passed'});
    }, function (allSuccessful) {
      scope.showStages = !allSuccessful;
    });

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
          Flash.notify('Success', 'Your change has been deleted.');
        })
        .catch(function (resp) {
          Flash.error('Error', 'There was an error. Your change may have not been deleted successfully.');
        });
    };

    scope.deliver = function () {
      Modal.open(
        'Confirmation',
        deliverConfirmationModalTemplate,
        'info-modal',
        scope
      );
    };

    scope.doDeliver = function (change) {
      Modal.close();
      // need to decide how the UI should behave after accepting a change
      change.deliver()
        .then(function (resp) {
          Flash.notify('Success', 'Your change has been delivered.');
        })
        .catch(function (resp) {
          Flash.error('Error', 'Your change may not have been delivered.');
        });
    };
  }

  return {
    template: changeCardTemplate,
    link: link,
    scope: {
      change: '='
    }
  };
}

export default ng
  .module('cd.components.changeCard', [
    Flash,
    Modal
  ])
  .directive('cdChangeCard', changeCardDirective)
  .name;
