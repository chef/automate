import ng from 'angular';
import { find } from 'lodash';
import uiRouter from 'angular-ui-router';
import filesController from './files_controller';
import filesComponent from './files_component';

export default ng
  .module('cd.routes.change.review.files', [
    uiRouter,
    filesController,
    filesComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.change.review.files', {
        url: '',
        controller: 'filesController',
        template: '<cd-files>',
        resolve: {
          patchset: ['$stateParams', 'change', function ($stateParams, change) {
            return find(change.patchsets, {
              sequenceNumber: parseInt($stateParams.end.split('p')[1])
            });
          }],
          comments: ['change', 'patchset', function (change, patchset) {
            return change.comments.$refresh({
              patchset: patchset.sequenceNumber
            }).$promise;
          }]
        },
        reloadOnSearch: false,
        onEnter: ['$stateParams', 'change', function ($stateParams, change) {
          this.data = {
            title:
              $stateParams.file + ' · ' + change.title + ' · ' + change.topic +
              '/' + change.target
          };
        }]
      });
  })
  .name;
