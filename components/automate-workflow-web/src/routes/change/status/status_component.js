import ng from 'angular';
import statusTemplate from './status.html';

function statusComponent() {
  return {
    template: statusTemplate
  };
}

export default ng
  .module('cd.routes.change.status.component', [])
  .directive('cdStatus', statusComponent)
  .name;
