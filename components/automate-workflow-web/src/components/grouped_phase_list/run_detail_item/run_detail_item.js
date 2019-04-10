import ng from 'angular';
import runDetailItemTemplate from './run_detail_item.html';
import capitalizeFilter from '../../../common/filters/capitalize';

runDetailItemDirective.inject = ['$http'];

function runDetailItemDirective($http) {

  function link(scope, element) {
    scope.open = false;
    scope.canCancel = false;

    $http.get(scope.runDetailItem.jobHref)
      .then((resp) => scope.canCancel = true,
            (error) => scope.canCancel = false);

    scope.toggle = function() {
      scope.open = !scope.open;
      element.toggleClass('open', scope.open);
      scope.$emit("phase_log_open_state_change", {
        href: scope.runDetailItem.href,
        open: scope.open
      });
    };

    // Note: see grouped_phase_list.js
    scope.jobIsCancelable = () =>
      scope.canCancel &&
      (scope.runDetailItem.status === 'idle' || scope.runDetailItem.status === 'running');

    scope.cancelPhase = () => {
      $http.delete(scope.runDetailItem.jobHref)
        .then((resp) => scope.canCancel = false);
    };

    scope.$on('close_other_logs', (eventData, href) => {
      if (scope.runDetailItem.href !== href) {
        scope.open = false;
        element.toggleClass('open', scope.open);
      }
    });

    scope.$on('close_all_logs', (e) => {
      scope.open = false;
    });
  }

  return {
    restrict: 'A',
    template: runDetailItemTemplate,
    link: link,
    scope: {
      runDetailItem: '=',
      phaseName: '='
    }
  };
}

export default ng
  .module('cd.components.groupedPhaseList.runDetailItem', [
    capitalizeFilter
  ])
  .directive('cdRunDetailItem', runDetailItemDirective)
  .name;
