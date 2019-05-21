import ng from 'angular';
import uiRouter from 'angular-ui-router';
import { find, includes } from 'lodash';
import sref from '../../helpers/sref';
import Session from '../../common/auth/session';
import Modal from '../../common/ui/modal/modal';
import confirmDeletedModalTemplate from '../../common/ui/modal/confirm_deleted_modal.html';
import Store from '../../common/store/store';

changeController.$inject = [
  '$scope',
  '$httpParamSerializer',
  '$state',
  '$window',
  '$interval',
  'Modal',
  'Session',
  'organization',
  'project',
  'change',
  'Store'
];

function changeController(
  $scope, $httpParamSerializer, $state, $window, $interval, Modal, Session,
  organization, project, change, Store) {

  $scope.organization = organization;
  $scope.project = project;
  $scope.change = change;

  // Construct stream URL
  let { username, token } = Session.get();
  let streamPath = `${change.$url()}/streaming`;
  let streamParams = $httpParamSerializer({
    'chef-delivery-user': username,
    'chef-delivery-token': token
  });
  let streamUrl = `${streamPath}?${streamParams}`;

  // Create the stream
  $scope.stream = undefined;
  let checkStream;

  // In the time between the controller first getting initialized and the
  // streaming connection being established, new change events could have
  // occurred so we want to make sure we fetch any new change data once a
  // connection is opened. Also, it's common for the connection to disconnect
  // and reconnect so doing this data fetch when a connection is established
  // ensures that any new change events that might have occurred during this
  // disconnect/reconnect period are still reflected to the user.
  $scope.handleStreamOpen = (e) => {
    $scope.$apply(() => change.$fetch());
  };

  $scope.handleBuildEvent = (e) => {
    $scope.$apply(() => change.$decode(JSON.parse(e.data)));
  };

  $scope.handleDeletedEvent = (e) => {
    $scope.$apply(() => {
      change.deletedBy = JSON.parse(e.data).deleted_by;
      Modal.open(
        'Change Deleted',
        confirmDeletedModalTemplate,
        'red-modal',
        $scope
      );
    });
  };

  $scope.handlePhaseRunUpdatedEvent = (e) => {
    $scope.$broadcast('phase_run_updated', JSON.parse(e.data));
  };

  $scope.confirmDeleted = () => {
    Modal.close();
    $state.go('main.enterprise.organizations.organization.project');
  };

  $scope.handleStreamError = (e) => {
    $scope.$apply(() => change.$fetch());
  };

  // If we are still promotable, update the promotion status. This check
  // prevents union status updates from updating superseded or delivered changes.
  $scope.handlePromotionStatusUpdate = (e) => {
    if ($scope.allowPromotion()) {
      $scope.$apply(() => {
        // Merge does not overwrite fields that no longer exist.
        // Delete the entire promotion object from the original state so
        // that old promotion fields do not persist when they shouldn't.
        delete change.promotion;
        angular.merge(change, JSON.parse(e.data));
      });
    }
  };

  $scope.initStream = function () {
    $scope.stream = new $window.EventSource(streamUrl);
    $scope.stream.addEventListener('open', $scope.handleStreamOpen);
    $scope.stream.addEventListener('build_event', $scope.handleBuildEvent);
    $scope.stream.addEventListener('change_deleted', $scope.handleDeletedEvent);
    $scope.stream.addEventListener('phase_run_updated', $scope.handlePhaseRunUpdatedEvent);
    $scope.stream.addEventListener('error', $scope.handleStreamError);
    $scope.stream.addEventListener('promotion_status_update', $scope.handlePromotionStatusUpdate);
  };

  $scope.closeStream = function () {
    $scope.stream.removeEventListener('open', $scope.handleStreamOpen);
    $scope.stream.removeEventListener('build_event', $scope.handleBuildEvent);
    $scope.stream.removeEventListener('change_deleted', $scope.handleDeletedEvent);
    $scope.stream.removeEventListener('phase_run_updated', $scope.handlePhaseRunUpdatedEvent);
    $scope.stream.removeEventListener('error', $scope.handleStreamError);
    $scope.stream.removeEventListener('promotion_status_update', $scope.handlePromotionStatusUpdate);
    $scope.stream.close();
  };

  $scope.diffView = Store.get('diffView') || 'unified';
  $scope.setDiffView = function(view) {
    $scope.diffView = view;
    Store.put('diffView', view, true);
  };

  $scope.promoteDiscouraged = () => {
    return ($scope.change.promotion.status === 'caution');
  };

  $scope.allowPromotion = () => {
    let { status } = $scope.change.promotion;
    let promotableStatuses = ['proceed', 'caution'];
    return includes(promotableStatuses, status);
  };

  $scope.promoteStatusReason = () => {
    let { reason } = $scope.change.promotion;
    return (!!reason ? reason : '');
  };

  $scope.promoteClass = () => {
    let { status } = $scope.change.promotion;
    return(status === 'proceed' ? '' : status);
  };

  $scope.promoteMessage = () => {
    let { reason } = $scope.change.promotion;
    let message = '';

    if ($scope.promoteDiscouraged() && reason === 'pipeline_union_failure') {
      message = 'Union stage failures.';
    }

    return message;
  };

  $scope.initStream();

  // Chrome browser is known to reset the readyState of an SSE to 2 after
  // computer sleep. We are using interval to check for a readyState of 2,
  // at which time we clean up the old SSE and start a new one.
  checkStream = $interval(() => {
    if ($scope.stream.readyState === 2 && !delivered()) {
      $scope.closeStream();
      $scope.initStream();
    }
  }, 5000);

  function delivered() {
    return !!find(change.stages, { stage: 'delivered', status: 'passed' });
  }

  $scope.$on('$destroy', () => {
    $scope.closeStream();
    $interval.cancel(checkStream);
  });


  $scope.$on('phase_log_open_state_change', (event, eventData) => {
    $scope.$broadcast('close_other_logs', eventData.href);
  });

  $scope.$on('closing_phase_log', (event, eventData) => {
    $scope.$broadcast('close_all_logs');
  });

  $scope.no_pr_url_text = () => {
    if ($scope.project.scm.type === 'bitbucket') {
      return 'There is a Bitbucket PR for this change';
    }
    if ($scope.project.scm.type === 'githubV2') {
      return 'There is a GitHub PR for this change';
    }
  };
}

export default ng
  .module('cd.routes.change.controller', [
    uiRouter,
    Modal,
    Session
  ])
  .controller('changeController', changeController)
  .name;
