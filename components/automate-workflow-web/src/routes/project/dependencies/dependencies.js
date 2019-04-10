import ng from 'angular';
import uiRouter from 'angular-ui-router';
import dependenciesController from './dependencies_controller';
import dependenciesComponent from './dependencies_component';

export default ng
  .module('cd.routes.project.dependencies', [
    uiRouter,
    dependenciesController,
    dependenciesComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.dependencies', {
        url: '/dependencies',
        controller: 'dependenciesController',
        template: '<cd-dependencies>'
      });
  })
  .name;
