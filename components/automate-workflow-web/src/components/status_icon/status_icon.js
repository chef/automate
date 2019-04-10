import ng from 'angular';
import { keys } from 'lodash';
import statusIconTemplate from './status_icon.html';

function statusIconDirective() {
  let icons = {
    running: 'gears',
    passed: 'checkmark-solo',
    failed: 'x',
    caution: 'exclamation',
    skipped: 'minus',
    idle: 'minus',
    waiting: 'human'
  };

  return {
    template: statusIconTemplate,
    transclude: true,
    scope: {
      statusIcon: '@'
    },
    link: (scope, element, attrs, ctrl, transclude) => {

      setStatus(scope.statusIcon);

      attrs.$observe('statusIcon', () => {
        setStatus(attrs.statusIcon);
      });

      transclude(scope.$parent, function(clone, scope) {
        element.append(clone);
      });

      function setStatus(status) {

        if (status) {
          scope.status = status;
          scope.href = `#icon-${icons[scope.status]}`;

          keys(icons).forEach((key) => {
            element.toggleClass(key, scope.status === key);
          });
        }
      }
    }
  };
}

export default ng
  .module('cd.common.ui.statusIcon', [])
  .directive('statusIcon', statusIconDirective)
  .name;
