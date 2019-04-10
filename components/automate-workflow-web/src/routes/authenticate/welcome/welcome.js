import ng from 'angular';
import uiRouter from 'angular-ui-router';
import welcomeController from './welcome_controller';
import welcomeComponent from './welcome_component';

export default ng
  .module('cd.routes.authenticate.welcome', [
    uiRouter,
    welcomeController,
    welcomeComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('authenticate.welcome', {
        url: '/welcome',
        template: '<cd-welcome>',
        controller: 'welcomeController'
      });
  })
  .name;
