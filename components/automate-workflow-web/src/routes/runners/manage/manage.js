import ng from 'angular';
import uiRouter from 'angular-ui-router';
import manageController from './manage_controller';
import manageComponent from './manage_component';

export default ng
  .module('cd.routes.runners.manage', [
    uiRouter,
    manageController,
    manageComponent,
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.runners.manage', {
        url: '/manage',
        template: '<cd-runners-manage runners="runners">',
        controller: 'manageController',
        resolve: {
          runners: ($http, ApiUrl) => {
            return $http
              .get(ApiUrl('/runners'))
              .then((resp) => resp.data,
                    (error) => []); // TODO: create custom Flash on error
          }
        }
      });
  })
  .name;
