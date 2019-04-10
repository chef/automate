import ng from 'angular';
import ngAnimate from 'angular-animate';

tabsetComponent.$inject = ['$animate'];

function tabsetComponent($animate) {

  function link(scope, element, attrs) {
    element.on('click', function (event) {
      var el = event.target.parentElement;

      if (el.classList.contains('cd-tab')) {
        element.children().removeClass('active-tab');
        el.classList.add('active-tab');
      }
    });
  }

  return {
    restrict: 'C',
    scope: {},
    link: link
  };
}

export default ng
  .module('cd.components.tabset', [
    ngAnimate
  ])
  .directive('cdTabset', tabsetComponent)
  .name;
