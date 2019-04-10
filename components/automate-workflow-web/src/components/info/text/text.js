import ng from 'angular';

function infoTextComponent() {

  function link(scope, element, attrs, infoCtrl, transcludeFn) {
    scope.show = function () {
      return infoCtrl.show;
    };
  }

  return {
    restrict: 'E',
    require: '^cdInfo',
    replace: true,
    transclude: true,
    link: link,
    template: '<div class="cd-info-text" ng-transclude ng-show="show()"></div>'
  };
}

export default ng
  .module('cd.components.info.text', [])
  .directive('cdInfoText', infoTextComponent)
  .name;
