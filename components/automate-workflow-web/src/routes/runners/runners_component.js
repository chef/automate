import ng from 'angular';
import runnersTemplate from './runners.html';

function runnersComponent() {
  return {
    template: runnersTemplate
  };
}

export default ng
  .module('cd.routes.runners.component', [])
  .directive('cdRunners', runnersComponent)
  .name;
