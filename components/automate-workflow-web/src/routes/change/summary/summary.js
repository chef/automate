import ng from 'angular';
import uiRouter from 'angular-ui-router';
import summaryController from './summary_controller';
import summaryComponent from './summary_component';

export default ng
  .module('cd.routes.change.summary', [
    uiRouter,
    summaryController,
    summaryComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.change.summary', {
        url: '/summary',
        controller: 'summaryController',
        template: '<cd-summary>',
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Summary'
          };
        }]
      });
  })
  .name;
