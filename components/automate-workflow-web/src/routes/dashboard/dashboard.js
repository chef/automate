import ng from 'angular';
import dashboardController from './dashboard_controller';
import dashboardComponent from './dashboard_component';
import Session from '../../common/auth/session';
import breadcrumbTemplate from './breadcrumb.html';

export default ng
  .module('cd.routes.dashboard', [
    dashboardController,
    dashboardComponent,
    Session
  ])
  .name;
