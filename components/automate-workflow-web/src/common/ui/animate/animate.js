import ng from 'angular';
import ngAnimate from 'angular-animate';

animateConfig.$inject = ['$animateProvider'];

function animateConfig($animateProvider) {
  // allows you to remove animation from any element by giving it
  // an `ng-animate-disabled` class name
  $animateProvider.classNameFilter(/^(?:(?!ng-animate-disabled).)*$/);
}

export default ng
  .module('common.ui.animate', [
    ngAnimate
  ])
  .config(animateConfig)
  .name;
