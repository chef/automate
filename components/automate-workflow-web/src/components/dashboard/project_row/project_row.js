import ng from 'angular';
import projectRowTemplate from './project_row.html';
import momentFilter from '../../../../src/common/filters/moment';
import storeService from '../../../../src/common/store/store';

function projectRowDirective() {

  function link(scope, element) {

    scope.toggle = () => {
      scope.project.open = !scope.project.open;
      scope.$emit('toggleProject', scope.project);
      toggleClass();
    };

    function toggleClass() {
      element.toggleClass('open', scope.project.open);
    }

    scope.applyFilter = (evt, expr) => {
      evt.stopPropagation();
      scope.$emit('filterClick', `project:${scope.project.name} ${expr}`);
    };

    toggleClass();
  }

  return {
    restrict: 'A',
    template: projectRowTemplate,
    link: link,
    scope: {
      project: '='
    }
  };
}

export default ng
  .module('cd.components.dashboard.projectRow', [
    momentFilter,
    storeService
  ])
  .directive('cdProjectRow', projectRowDirective)
  .name;
