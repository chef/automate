import ng from 'angular';
import iconButtonTemplate from './icon_button.html';

function iconButtonDirective() {

  return {
    template: iconButtonTemplate,
    transclude: true,
    scope: {
      iconButton: '@'
    },
    link: (scope, element, attrs, ctrl, transclude) => {
      setIcon(scope.iconButton);

      attrs.$observe('iconButton', () => {
        setIcon(attrs.iconButton);
      });

      function setIcon(icon) {
        if (icon) {
          scope.href = `#icon-${icon}`;
        }
      }

      transclude(scope.$parent, function(clone, scope) {
        element.addClass('button');
        element.append(clone);
      });
    }
  };
}

export default ng
  .module('cd.common.ui.iconButton', [])
  .directive('iconButton', iconButtonDirective)
  .name;
