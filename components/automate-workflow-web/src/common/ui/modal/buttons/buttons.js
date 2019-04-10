import ng from 'angular';

modalButtonsDirective.$inject = [];

function modalButtonsDirective() {
  return {
    restrict: 'E',
    transclude: true,
    scope: true,
    template: '<div class="cd-modal-buttons" ng-transclude></div>'
  };
}

export default ng
  .module('cd.common.ui.modal.buttons', [])
  .directive('cdModalButtons', modalButtonsDirective)
  .name;
