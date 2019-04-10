import ng from 'angular';
import changeTemplate from './change.html';

function changeComponent() {
  return {
    template: changeTemplate
  };
}

export default ng
  .module('cd.routes.change.component', [])
  .directive('cdChange', changeComponent)
  .name;
