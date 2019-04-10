import ng from 'angular';
import cloneTemplate from './clone.html';

function cloneComponent() {
  return {
    template: cloneTemplate
  };
}

export default ng
  .module('cd.routes.project.clone.component', [])
  .directive('cdClone', cloneComponent)
  .name;
