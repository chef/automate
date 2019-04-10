import ng from 'angular';
import capitalizeFilter from '../../common/filters/capitalize';
import groupedPhaseListTemplate from './grouped_phase_list.html';

groupedPhaseListComponent.$inject = ['$http'];

function groupedPhaseListComponent($http) {

  function link(scope) {
    scope.open = false;
    scope.logOpen = false;
    scope.canCancel = false;

    // If there're sub-jobs (i.e., runDetails.length > 1), run_detail_item
    // takes care of the cancel buttons.
    if(scope.phase.runDetails.length > 1) {
      scope.canCancel = false;
    } else {
      $http.get(scope.phase.runDetails[0].jobHref)
        .then((resp) => scope.canCancel = true,
              (error) => scope.canCancel = false);
    }

    scope.toggle = () => {
      if (scope.phase.runDetails.length === 1) {
        scope.logOpen = !scope.logOpen;

        scope.$emit('phase_log_open_state_change', {
          href: scope.phase.runDetails[0].href,
          open: scope.logOpen
        });

      } else {
        scope.open = !scope.open;
      }
    };

    // Note: this is for checking if it might have been a job that could have
    // been canceled, but now it's failed/passed (and we don't send another
    // request to check if the job no longer has a 200 response).
    scope.jobIsCancelable = () =>
      scope.canCancel &&
      (scope.phase.status === 'idle' || scope.phase.status === 'running');

    scope.cancelPhase = () => {
      $http.delete(scope.phase.runDetails[0].jobHref)
        .then((resp) => scope.canCancel = false);
    };

    scope.$on('close_other_logs', (eventData, href) => {
      if (scope.phase.runDetails[0].href !== href) {
        scope.logOpen = false;
      }
    });

    scope.$on('close_all_logs', (e) => {
      scope.logOpen = false;
    });
  }

  return {
    link: link,
    template: groupedPhaseListTemplate,
    scope: {
      phase: '=cdGroupedPhaseList',
    }
  };
}

export default ng
  .module('cd.components.groupedPhaseList', [
    capitalizeFilter
  ])
  .directive('cdGroupedPhaseList', groupedPhaseListComponent)
  .name;
