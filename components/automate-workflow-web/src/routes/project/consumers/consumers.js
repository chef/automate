import ng from 'angular';
import uiRouter from 'angular-ui-router';
import consumersController from './consumers_controller';
import consumersComponent from './consumers_component';

export default ng
  .module('cd.routes.project.consumers', [
    uiRouter,
    consumersController,
    consumersComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.consumers', {
        url: '/consumers',
        controller: 'consumersController',
        template: '<cd-consumers>'
      });
  })
  .name;
