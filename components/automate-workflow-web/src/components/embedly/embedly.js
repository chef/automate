import ng from 'angular';
import ngEmbedly from 'angular-embedly/angular.embedly';
import embedlyController from './embedly_controller';

function embedlyDirective() {
  return {
    controller: 'embedlyController'
  };
}

export default ng
  .module('cd.components.embedly', [
    'ngEmbedApp',
    embedlyController
  ])
  .directive('cdEmbedly', embedlyDirective)
  .name;
