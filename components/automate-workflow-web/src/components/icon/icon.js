import ng from 'angular';
import iconTemplate from './icon.html';

function iconDirective() {

  return {
    template: iconTemplate,
    scope: {
      icon: '@'
    },
    link: (scope, element, attrs) => {

      setIcon(scope.icon);

      attrs.$observe('icon', () => {
        setIcon(attrs.icon);
      });

      function setIcon(icon) {

        if (icon) {
          scope.href = `#icon-${icon}`;
        }
      }
    }
  };
}

export default ng
  .module('cd.common.ui.icon', [])
  .directive('icon', iconDirective)
  .name;
