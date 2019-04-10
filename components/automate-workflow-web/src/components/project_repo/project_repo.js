import ng from 'angular';
import { first, omit } from 'lodash';
import projectRepoTemplate from './project_repo.html';

projectRepoComponent.$inject = [
  '$log'
];

function projectRepoComponent($log) {

  function link(scope, element, attrs) {

    scope.selectProvider = (prov) => {
      let provider = omit(prov, 'name');

      scope.selectedProvider = provider;

      if (scope.project) {
        scope.project.scm = provider;
      }
    };

    initProvider();

    function initProvider() {
      scope.selectProvider(scope.project && scope.project.scm ? scope.project.scm : first(scope.providers));
    }

    scope.$watch('project', (project) => {
      if (project && !project.scm) {
        project.scm = scope.selectedProvider;
      }
    });
  }

  return {
    link: link,
    scope: {
      org: '=',
      project: '=',
      providers: '='
    },
    template: projectRepoTemplate
  };
}

export default ng
  .module('cd.components.projectRepo', [])
    .directive('cdProjectRepo', projectRepoComponent)
    .name;
