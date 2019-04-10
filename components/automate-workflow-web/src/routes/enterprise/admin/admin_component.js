import ng from 'angular';
import adminTemplate from './admin.html';

function adminComponent() {
  return {
    template: adminTemplate
  };
}

export default ng
  .module('cd.routes.enterprise.admin.component', [])
  .directive('cdAdmin', adminComponent)
  .name;
