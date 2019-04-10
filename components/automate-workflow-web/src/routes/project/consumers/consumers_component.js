import ng from 'angular';
import consumersTemplate from './consumers.html';

function consumersComponent() {
  return {
    template: consumersTemplate
  };
}

export default ng
  .module('cd.routes.project.consumers.component', [])
  .directive('cdConsumers', consumersComponent)
  .name;
