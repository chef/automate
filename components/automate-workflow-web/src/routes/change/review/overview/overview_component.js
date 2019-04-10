import ng from 'angular';
import bsCollapse from 'angular-strap/dist/modules/collapse';
import overviewTemplate from './overview.html';

function overviewComponent() {
  return {
    template: overviewTemplate
  };
}

export default ng
  .module('cd.routes.change.review.overview.component', [
    'mgcrea.ngStrap.collapse',
  ])
  .directive('cdOverview', overviewComponent)
  .name;
