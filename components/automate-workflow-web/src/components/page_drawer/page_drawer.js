import ng from 'angular';

function pageDrawerComponent() {

  function link(scope, element, attrs) {
    element.addClass('ng-animate-disabled');
  }

  return {
    link: link
  };
}

export default ng
  .module('cd.components.pageDrawer', [])
  .directive('cdPageDrawer', pageDrawerComponent)
  .name;
