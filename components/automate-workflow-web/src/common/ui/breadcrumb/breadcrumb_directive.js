import ng from 'angular';
import Store from '../../store/store';
import breadcrumbController from './breadcrumb_controller';
import breadcrumbTemplate from './breadcrumb.html';

breadcrumbDirective.$inject = ['Store'];

function breadcrumbDirective(Store) {

  return {
    template: breadcrumbTemplate,
    scope: {
      breadcrumb: '=cdBreadcrumb'
    }
  };
}

export default ng
  .module('cd.common.ui.breadcrumb.directive', [
    Store,
    breadcrumbController
  ])
  .directive('cdBreadcrumb', breadcrumbDirective)
  .name;
