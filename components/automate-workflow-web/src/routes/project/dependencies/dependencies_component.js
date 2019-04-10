import ng from 'angular';
import dependenciesTemplate from './dependencies.html';

function dependenciesComponent() {
  return {
    template: dependenciesTemplate
  };
}

export default ng
  .module('cd.routes.project.dependencies.component', [])
  .directive('cdDependencies', dependenciesComponent)
  .name;
