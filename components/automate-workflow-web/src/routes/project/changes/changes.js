import ng from 'angular';
import uiRouter from 'angular-ui-router';
import changesController from './changes_controller';
import changesComponent from './changes_component';

export default ng
  .module('cd.routes.project.changes', [
    uiRouter,
    changesController,
    changesComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.changes', {
        url: '/changes',
        controller: 'changesController',
        template: '<cd-changes>'
      });
  })
  .name;
