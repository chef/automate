import ng from 'angular';
import organizationTemplate from './organization.html';

function organizationComponent() {
  return {
    template: organizationTemplate
  };
}

export default ng
  .module('cd.routes.organization.component', [])
  .directive('cdOrganization', organizationComponent)
  .name;
