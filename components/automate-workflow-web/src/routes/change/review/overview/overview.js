import ng from 'angular';
import uiRouter from 'angular-ui-router';
import overviewController from './overview_controller';
import overviewComponent from './overview_component';

export default ng
  .module('cd.routes.change.review.overview', [
    uiRouter,
    overviewController,
    overviewComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.change.review.overview', {
        url: '',
        controller: 'overviewController',
        template: '<cd-overview>',
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Review'
          };
        }]
      });
  })
  .name;
