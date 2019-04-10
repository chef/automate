import ng from 'angular';
import diffStatsTemplate from './diff_stats.html';

function diffStatsDirective() {

  return {
    template: diffStatsTemplate,
    scope: {
      insertions: '=',
      deletions: '='
    },
    link: (scope, element, attrs) => {

      scope.hasStats = () => {
        return !isNaN(parseInt(scope.insertions)) && !isNaN(parseInt(scope.deletions));
      };

      scope.total = () => {
        return (parseInt(scope.insertions) + parseInt(scope.deletions));
      };

      scope.percentage = (stat) => {
        return (parseInt(stat) / scope.total()) * 100;
      };

      scope.width = (stat) => {
        return {
          'width': scope.percentage(stat) + '%'
        };
      };
    }
  };
}

export default ng
  .module('cd.common.ui.diffStats', [])
  .directive('diffStats', diffStatsDirective)
  .name;
