import ng from 'angular';
import changeRowTemplate from './change_row.html';

function changeRowDirective() {
  return {
    restrict: 'A',
    template: changeRowTemplate,
    scope: {
      change: '=change'
    },
    link: (scope) => {

      let stages = {
        'verify': 0,
        'build': 1,
        'acceptance': 2,
        'union': 3,
        'rehearsal': 4,
        'delivered': 5
      };

      scope.prior = (stage) => {
        return stages[scope.change.stage] > stages[stage];
      };

      scope.approvable = () => {
        return scope.statusForStage('verify') === 'passed' && !scope.change.approved_by;
      };

      scope.approved = () => {
        return !!scope.change.approved_by;
      };

      scope.deliverable = () => {
        return scope.statusForStage('acceptance') === 'passed' && !scope.change.delivered_by;
      };

      scope.delivered = () => {
        return !!scope.change.delivered_by;
      };

      scope.statusForStage = (stage) => {
        let status;

        if (scope.prior(stage)) {
          status = 'passed';
        }
        else if (scope.change.stage === stage) {
          status = scope.change.stage_status;
        }

        return status;
      };

      scope.titleForStage = (stage) => {
        return `${scope.statusForStage(stage)} in ${stage}`;
      };

      scope.classNamePrior = (stage, passed) => {
        if (scope.prior(stage) || passed) {
          return 'progress';
        } else {
          return scope.className(stage);
        }
      };

      scope.className = (stage) => {
        let rawStatus = scope.statusForStage(stage);
        if (rawStatus === 'passed') {
          return 'waiting';
        } else {
          return rawStatus;
        }
      };

      scope.deliveredClass = () => {
        let status = scope.statusForStage('delivered');
        if (status === 'passed') {
          return 'complete';
        } else {
          return status;
        }
      };

    }
  };
}

export default ng
  .module('cd.components.dashboard.changeRow', [])
  .directive('cdChangeRow', changeRowDirective)
  .name;
