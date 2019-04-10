import ng from 'angular';
import authenticateTemplate from './authenticate.html';

function authenticateComponent() {
  return {
    template: authenticateTemplate,
  };
}

export default ng
  .module('cd.routes.authenticate.component', [])
  .directive('cdAuthenticate', authenticateComponent)
  .name;
