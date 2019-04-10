import ng from 'angular';
import selectTemplate from './select.html';
import option from './option/option';

function selectComponent() {
  function link(scope, element, attrs, ngModelCtrl) {
    element.on('click', function() {
      element.toggleClass('open');
    });
  }

  return {
    link: link,
    require: 'ngModel',
    scope: {
      selection: '=ngModel'
    },
    template: selectTemplate,
    transclude: true
  };
}

export default ng
  .module('cd.components.select', [
    option
  ])
  .directive('cdSelect', selectComponent)
  .name;
