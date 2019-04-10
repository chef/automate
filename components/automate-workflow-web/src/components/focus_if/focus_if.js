import ng from 'angular';

function focusIfComponent() {

  function link(scope, element, attrs) {
    if (scope.focus) {
      element[0].focus();
    }
  }

  return {
    restrict: 'A',
    scope: {
      focus: '=cdFocusIf'
    },
    link: link
  };
}

export default ng
  .module('cd.components.focusIf', [])
  .directive('cdFocusIf', focusIfComponent)
  .name;
