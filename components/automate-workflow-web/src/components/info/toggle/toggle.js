import ng from 'angular';

function infoToggleComponent() {

  function link(scope, element, attrs, infoCtrl, transcludeFn) {
    element.on('click', infoCtrl.toggle);
  }

  return {
    restrict: 'E',
    require: '^cdInfo',
    replace: true,
    transclude: true,
    link: link,
    template: '<span class="cd-info-toggle" ng-transclude></span>'
  };
}

export default ng
  .module('cd.components.info.toggle', [])
  .directive('cdInfoToggle', infoToggleComponent)
  .name;
