import ng from 'angular';
import uiRouter from 'angular-ui-router';
import auditController from './audit_controller';
import auditComponent from './audit_component';

export default ng
  .module('cd.routes.audit', [
    uiRouter,
    auditController,
    auditComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.audit', {
        url: '/audit',
        reloadOnSearch: false,
        views: {
          'tabbed': {
            template: '<cd-audit>',
            controller: 'auditController'
          }
        },
        resolve: {
          auditLog: function ($http, ApiUrl) {
            return $http
              .get(ApiUrl('/audit'))
              .then((resp) => resp.data);
          }
        }
      });
  })
  .name;
