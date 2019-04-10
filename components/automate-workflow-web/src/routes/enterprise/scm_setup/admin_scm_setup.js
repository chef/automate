import ng from 'angular';
import { some } from 'lodash';
import { values } from 'lodash';
import uiRouter from 'angular-ui-router';
import scmSetupController from './scm_setup_controller';
import scmSetupComponent from './scm_setup_component';

export default ng
  .module('cd.routes.admin.scm_setup', [
    uiRouter,
    scmSetupController,
    scmSetupComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.admin.scm_setup', {
        url: 'scm-setup',
        resolve: {
          scmProviders: ['ApiUrl', function (ApiUrl) {
            return [
              {
                name: 'Bitbucket',
                type: 'bitbucket',
                setupUrl: ApiUrl('/scm/bitbucket/servers')
              },
              {
                name: 'GitHub',
                type: 'github',
                setupUrl: ApiUrl('/scm/github/servers')
              }
            ];
          }],
          // This changes when we have multiple configs for each right now we
          // are assuming one so we go get the list and populate the Scope
          // based of the first element. In a future world you might click
          // edit on one item in the list and then we go hit the self link.
          scmSetup: ['$http', '$q', 'scmProviders', function ($http, $q, scmProviders) {
            return $q.all(scmProviders
              .filter((provider) => provider.setupUrl !== 'not-implemented')
              .reduce((promises, provider) => {
                let promise = $http
                  .get(provider.setupUrl)
                  .then((resp) => {
                    if (resp.data[0]) {
                      return resp.data[0];
                    }
                  })
                  .catch((resp) => {
                    if (resp.status === 403) {
                       return {forbidden: true};
                    }
                  });
                promises[provider.type] = promise;
                return promises;
              }, {}));
          }],
          authorized: ['scmSetup', function (scmSetup) {
            // If any SCM Setup endpoint returns 403 then we are not authorized.
            return !some(values(scmSetup), 'forbidden');
          }]
        },
        views: {
          'tabbed': {
            template: '<cd-scm-setup>',
            controller: 'scmSetupController'
          }
        }
      });
  })
  .name;
