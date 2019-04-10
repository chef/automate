import ng from 'angular';
import uiRouter from 'angular-ui-router';
import pipelinesController from './pipelines_controller';
import pipelinesComponent from './pipelines_component';

export default ng
  .module('cd.routes.project.pipelines', [
    uiRouter,
    pipelinesController,
    pipelinesComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.pipelines', {
        url: '/pipelines',
        controller: 'pipelinesController',
        template: '<cd-pipelines>'
      });
  })
  .name;
