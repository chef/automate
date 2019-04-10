import ng from 'angular';
import uiRouter from 'angular-ui-router';
import cloneComponent from './clone_component';

export default ng
  .module('cd.routes.project.clone', [
    uiRouter,
    cloneComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.clone', {
        url: '/clone',
        template: '<cd-clone>'
      });
  })
  .name;
