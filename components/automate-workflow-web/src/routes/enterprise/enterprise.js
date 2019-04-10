import ng from 'angular';
import uiRouter from 'angular-ui-router';
import enterpriseController from './enterprise_controller';
import enterpriseComponent from './enterprise_component';
import enterpriseSearchesController from './enterprise_searches_controller';
import smtpSetupController from './smtp_setup/smtp_setup_controller';
import samlSetupController from './saml_setup/saml_setup_controller';
import Organization from '../../common/models/organization';
import Session from '../../common/auth/session';
import User from '../../common/models/user';
import admin from './admin/admin';
import dashboard from '../dashboard/dashboard';

export default ng
  .module('cd.routes.enterprise', [
    uiRouter,
    enterpriseController,
    enterpriseComponent,
    enterpriseSearchesController,
    smtpSetupController,
    samlSetupController,
    Organization,
    Session,
    admin,
    dashboard
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise', {
        abstract: true,
        views: {
          'main': {
            template: '<cd-enterprise>'
          }
        }
      })
      .state('main.enterprise.dashboard', {
        url: '/dashboard?token',
        views: {
          'tabbed': {
            template: '<cd-dashboard>',
            controller: 'dashboardController'
          }
        }
      })
      .state('main.enterprise.organizations', {
        url: '/organizations',
        views: {
          'tabbed': {
            template: '<cd-organizations class="organizations" orgs="orgs">',
            controller: 'enterpriseController'
          }
        },
        resolve: {
          orgs: ['Organization', function (Organization) {
            return Organization.$collection().$refresh().$promise;
          }]
        }
      })
      .state('main.enterprise.users', {
        url: '/users',
        views: {
          'tabbed': {
            template: '<cd-users class="users" users="users">'
          }
        },
      })
      .state('main.enterprise.searches', {
        url: '/searches',
        resolve: {
          searches: function ($http, ApiUrl) {
            return $http.get(ApiUrl('/searches')).then((resp) => resp.data);
          }
        },
        views: {
          'tabbed': {
            template: '<cd-searches class="searches">',
            controller: 'enterpriseSearchesController'
          }
        }
      })
      .state('main.enterprise.smtp_setup', {
        url: '/smtp-setup',
        resolve: {
          currentUser: ['$http', 'CurrentUser', ($http, CurrentUser) => {
            return CurrentUser.user();
          }],
          smtp: ['currentUser', '$http', 'ApiUrl', 'Session', (currentUser, $http, ApiUrl, Session) => {
            return $http.get(ApiUrl('/notifications/smtp'))
              .then((resp) => {
                  return {
                    authorized: true,
                    config: resp.data,
                    user: currentUser
                  };
                }, (resp) => {
                  return {
                    authorized: resp.status !== 403,
                    config: null,
                    user: currentUser
                  };
                });
          }]
        },
        views: {
          'tabbed': {
            template: '<cd-smtp-setup class="smtp-setup" smtp="smtp">',
            controller: 'smtpSetupController'
          }
        },
      })
      .state('main.enterprise.saml_setup', {
        url: '/saml-setup',
        resolve: {
          saml: ['$http', 'ApiUrl', ($http, ApiUrl) => {
            return $http.get(ApiUrl('/saml/config'))
              .then((resp) => {
                  return {
                    authorized: true,
                    config: resp.data
                  };
                })
                .catch((resp) => {
                  return {
                    authorized: resp.status !== 403,
                    config: null
                  };
                });
          }]
        },
        views: {
          'tabbed': {
            template: '<cd-saml-setup class="saml-setup" saml="saml">',
            controller: 'samlSetupController'
          }
        },
      });
  })
  .name;
