import ng from 'angular';
import uiRouter from 'angular-ui-router';
import statusController from './status_controller';
import statusComponent from './status_component';
import verifyTemplate from './status/verify.html';
import buildTemplate from './status/build.html';
import acceptanceTemplate from './status/acceptance.html';
import unionTemplate from './status/union.html';
import rehearsalTemplate from './status/rehearsal.html';
import deliveredTemplate from './status/delivered.html';

export default ng
  .module('cd.routes.change.status', [
    uiRouter,
    statusController,
    statusComponent
  ])
  .config(($stateProvider) => {
    let stateParent = 'main.enterprise.organizations';
    $stateProvider
      .state(`${stateParent}.organization.project.change.status`, {
        url: '/status',
        controller: 'statusController',
        template: '<cd-status>',
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.verify`, {
        url: '/verify',
        template: verifyTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.build`, {
        url: '/build',
        template: buildTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.acceptance`, {
        url: '/acceptance',
        template: acceptanceTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.union`, {
        url: '/union',
        template: unionTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.rehearsal`, {
        url: '/rehearsal',
        template: rehearsalTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      })
      .state(`${stateParent}.organization.project.change.status.delivered`, {
        url: '/delivered',
        template: deliveredTemplate,
        onEnter: ['change', function (change) {
          this.data = {
            title: change.title + ' · ' +
              change.topic + '/' + change.target + ' · Status'
          };
        }]
      });
  })
  .name;
