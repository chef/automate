import ng from 'angular';
import uiRouter from 'angular-ui-router';
import runnersController from './runners_controller';
import runnersComponent from './runners_component';
import manage from './manage/manage';
import queue from './queue/queue';

export default ng
  .module('cd.routes.runners', [
    uiRouter,
    runnersController,
    runnersComponent,
    manage,
    queue
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.runners', {
        url: '/runners',
        reloadOnSearch: false,
        views: {
          'tabbed': {
            template: '<cd-runners>',
            controller: 'runnersController'
          }
        }
      });
  })
  .name;
