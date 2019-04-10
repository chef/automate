import ng from 'angular';
import ngScroll from 'angular-scroll';
import uiRouter from 'angular-ui-router';
import bsCollapse from 'angular-strap/dist/modules/collapse';
import { find, filter } from 'lodash';
import Flash from '../../common/ui/flash/flash';
import Modal from '../../common/ui/modal/modal';
import patchsetCardTemplate from './patchset_card.html';
import approveConfirmationModalTemplate
  from '../../common/ui/modal/approve_confirmation_modal.html';

patchsetCardComponent.$inject = [
  'Flash',
  'Modal',
  '$location',
  '$document',
  '$timeout',
  '$interval',
  '$state'
];

function patchsetCardComponent(
  Flash, Modal, $location, $document, $timeout, $interval, $state) {

  function link(scope, element, attrs) {

    scope.patchsetComments = scope.change.comments.$collection();

    function getComments() {
      return scope.patchsetComments.$refresh({
        patchset: scope.patchset.sequenceNumber
      });
    }

    getComments();

    // Poll for new comments every 3 seconds
    var poll = $interval(getComments, 3000);

    scope.$on('$destroy', function () {
      $interval.cancel(poll);
    });

    scope.closeModal = function () {
      Modal.close();
    };

    scope.doApprove = function (change) {
      Modal.close();
      change.approve()
        // need to decide how the UI should behave after approving a change
        .then(function (resp) {
          $state.go('main.enterprise.organizations.organization.project.change.status');
        })
        .catch(function (resp) {
          Flash.error('Error', 'Change failed to merge.');
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

    scope.getPatchsetComments = function (patchset) {
      return filter(scope.patchsetComments, {
        type: 'patchset',
      });
    };

    scope.filesWithComments = function (patchset) {
      return patchset.filesChanged.filter(function (file) {
        return filter(scope.patchsetComments, {
          type: 'line',
          file: file[1]
        }).length > 0;
      });
    };

    scope.scrollToComment = function (comment) {
      return $timeout(function () {
        $document.scrollTo(
          $document[0].getElementById('comment-' + comment.id), 60, 400
        );
      });
    };

    var highlightedComment = find(scope.getPatchsetComments(scope.patchset),
      function (comment) {
        return $location.hash() === 'comment-' + comment.id;
      }
    );

    if (highlightedComment) {
      scope.showComments = true;
      scope.scrollToComment(highlightedComment);
    }
  }

  return {
    template: patchsetCardTemplate,
    link: link
  };
}

export default ng
  .module('cd.components.patchsetCard', [
    uiRouter,
    'duScroll',
    'mgcrea.ngStrap.collapse',
    Flash,
    Modal
  ])
  .directive('cdPatchsetCard', patchsetCardComponent)
  .name;
