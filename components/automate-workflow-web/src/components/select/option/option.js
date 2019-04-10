import ng from 'angular';

function optionComponent() {
  function link(scope, element, attrs, ngModelCtrl) {
    element.addClass('cd-option');

    element.on('click', function () {
      ngModelCtrl.$setViewValue(ng.copy(scope.value));
    });
  }

  return {
    link: link,
    require: '^ngModel',
    scope: {
      value: '='
    }
  };
}

export default ng
  .module('cd.components.select.option', [])
  .directive('cdOption', optionComponent)
  .name;
