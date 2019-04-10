import ng from 'angular';
import uiRouter from 'angular-ui-router';
import reviewComponent from './review_component';
import overview from './overview/overview';
import files from './files/files';

export default ng
  .module('cd.routes.change.review', [
    uiRouter,
    reviewComponent,
    overview,
    files
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project.change.review', {
        url: '/review?file&start&end',
        template: '<cd-review>',
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Review'
          };
        }]
      });
  })
  .name;
