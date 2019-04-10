import ng from 'angular';
import text from './text/text';
import toggle from './toggle/toggle';

function infoComponent() {

  controller.$inject = ['$scope'];

  function controller($scope) {
    var self = this;

    self.show = false;

    self.toggle = function () {
      $scope.$apply(function () {
        self.show = !self.show;
      });
    };
  }

  return {
    restrict: 'E',
    template: '<div class="cd-info" ng-transclude></div>',
    scope: {},
    replace: true,
    controller: controller,
    transclude: true
  };
}

export default ng
  .module('cd.components.info', [
    text,
    toggle
  ])
  .directive('cdInfo', infoComponent)
  .name;
