import ng from 'angular';
import uiRouter from 'angular-ui-router';
import ngGravatar from 'angular-gravatar';
import mainController from './main_controller';
import mainTemplate from './main.html';

export default ng
  .module('cd.routes.main', [
    uiRouter,
    'ui.gravatar',
    mainController
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main', {
        abstract: true,
        template: mainTemplate,
        controller: 'mainController'
      });
  })
  .name;
