import ng from 'angular';
import changesTemplate from './changes.html';

function changesComponent() {
  return {
    template: changesTemplate
  };
}

export default ng
  .module('cd.routes.project.changes.component', [])
  .directive('cdChanges', changesComponent)
  .name;
