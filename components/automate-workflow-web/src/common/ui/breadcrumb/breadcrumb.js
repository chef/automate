import ng from 'angular';
import uiRouter from 'angular-ui-router';
import BreadcrumbService from './breadcrumb_service';
import BreadcrumbDirective from './breadcrumb_directive';

export default ng
  .module('cd.common.ui.breadcrumb', [
    uiRouter,
    BreadcrumbService,
    BreadcrumbDirective
  ])
  .name;
