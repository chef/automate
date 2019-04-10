import ng from 'angular';
import scmSetupTemplate from './scm_setup.html';

function scmSetupComponent() {
  return {
    template: scmSetupTemplate
  };
}

export default ng
  .module('cd.routes.enterprise.scm_setup.component', [])
  .directive('cdScmSetup', scmSetupComponent)
  .name;
