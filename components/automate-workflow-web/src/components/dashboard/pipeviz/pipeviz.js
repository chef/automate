import ng from 'angular';
import pipevizTemplate from './pipeviz.html';

function pipevizDirective() {
  return {
    link: (scope) => {

      scope.idleAddition = (status, count) => {
        return count === 0 ? status + "-idle" : status;
      };

      scope.applyFilter = (expr) => {
        scope.$emit('filterClick', expr);
      };

      scope.urdClassName = (status) => {
        if (status === "running") {
          return "running-urd";
        } else {
          return status;
        }
      };
    },
    scope: {
      pipeStats: '=cdPipeviz'
    },
    template: pipevizTemplate
  };
}

export default ng
  .module('cd.components.dashboard.pipeviz', [])
  .directive('cdPipeviz', pipevizDirective)
  .name;
