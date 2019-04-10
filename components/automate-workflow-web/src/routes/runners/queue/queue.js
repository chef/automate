import ng from 'angular';
import uiRouter from 'angular-ui-router';
import queueController from './queue_controller';
import queueComponent from './queue_component';

export default ng
  .module('cd.routes.runners.queue', [
    uiRouter,
    queueController,
    queueComponent,
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.runners.queue', {
        url: '/jobs',
        template: '<cd-jobs-queue jobs="jobs">',
        controller: 'queueController',
        resolve: {
          jobs: ($http, ApiUrl) => {
            return $http
              .get(ApiUrl('/jobs'))
              .then((resp) => resp.data,
                    (error) => []);
          }
        }
      });
  })
  .name;
