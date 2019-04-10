import ng from 'angular';
import uiRouter from 'angular-ui-router';
import changeController from './change_controller';
import changeComponent from './change_component';
import status from './status/status';
import review from './review/review';
import summary from './summary/summary';
import breadcrumbTemplate from './breadcrumb.html';

export default ng
  .module('cd.routes.change', [
    uiRouter,
    status,
    review,
    summary,
    changeController,
    changeComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.change', {
        url: '/changes/:change',
        views: {
          'tabbed@main.enterprise': {
            controller: 'changeController',
            template: '<cd-change>'
          }
        },
        resolve: {
          change: ['$stateParams', 'project', function ($stateParams, project) {
            return project.changes.$find($stateParams.change).$promise;
          }],
          comments: ['change', function (change) {
            return change.comments.$refresh({
              patchset: change.patchsets[0].sequenceNumber
            }).$promise;
          }]
        },
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' + change.topic + '/' + change.target
          };
        }]
      });
  })
  .name;
