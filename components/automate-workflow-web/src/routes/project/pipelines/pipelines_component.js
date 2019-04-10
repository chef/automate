import ng from 'angular';
import pipelinesTemplate from './pipelines.html';

function pipelinesComponent() {
  return {
    template: pipelinesTemplate
  };
}

export default ng
  .module('cd.routes.project.pipelines.component', [])
  .directive('cdPipelines', pipelinesComponent)
  .name;
