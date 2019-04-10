import ng from 'angular';
import uiRouter from 'angular-ui-router';
import breadcrumbTemplate from '../breadcrumb.html';
import adminController from './admin_controller';
import adminComponent from './admin_component';
import scmSetup from '../scm_setup/admin_scm_setup';
import smtpSetupController from '../smtp_setup/smtp_setup_controller';
import samlSetupController from '../saml_setup/saml_setup_controller';
import teamsController from '../teams/teams_controller';
import teamController from '../teams/team_controller';
import Session from '../../../common/auth/session';
import TeamsService from '../../../common/teams/teams_service';

export default ng
  .module('cd.routes.admin', [
    uiRouter,
    adminController,
    adminComponent,
    scmSetup,
    smtpSetupController,
    samlSetupController,
    teamsController,
    teamController,
    Session,
    TeamsService
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.admin', {
        url: '/',
        views: {
          'main': {
            template: '<cd-admin>',
            controller: 'adminController'
          },
          'breadcrumb': {
            template: breadcrumbTemplate,
            controller: 'breadcrumbController'
          }
        },
        resolve: {
          users: ['User', function (User) {
            return User.$collection().$refresh().$promise;
          }]
        }
      })
      .state('main.admin.users', {
        url: 'users',
        views: {
          'tabbed': {
            template: '<cd-users class="users" users="users">'
          }
        }
      })
      .state('main.admin.teams', {
        url: 'teams',
        resolve: {
          teams: ['$http', 'ApiUrl', 'TeamsService', ($http, ApiUrl, TeamsService) => {
            return TeamsService.fetch()
              .then(() => {
                return TeamsService.teams;
              })
              .catch(() => {
                return [];
              });
          }],
          isAdmin: ['$http', 'ApiUrl', 'Session', ($http, ApiUrl, Session) => {
            var userName = Session.get('username');
            return $http.get(ApiUrl('/authz/users/' + userName))
              .then(() => true)
              .catch(() => false);
          }]
        },
        views: {
          'tabbed': {
            template: '<cd-teams class="teams" teams="teams">',
            controller: 'teamsController'
          }
        }
      })
      .state('main.admin.teams.team', {
        url: '/:team',
        views: {
          'tabbed@main.admin': {
            template: '<cd-team class="team" team="team">',
            controller: 'teamController'
          }
        },
        resolve: {
          team: ['$http','$stateParams', 'ApiUrl', function($http, $stateParams, ApiUrl) {
            return $http.get(ApiUrl(`/teams/${$stateParams.team}`))
              .then((resp) => {
                return resp.data;
              });
          }],
          members: ['$http','$stateParams', 'ApiUrl', function($http, $stateParams, ApiUrl) {
            return $http.get(ApiUrl(`/teams/${$stateParams.team}/members`))
              .then((resp) => {
                return resp.data.members;
              })
              .catch(() => {
                return [];
              });
          }],
          users: ['$http','$stateParams', 'ApiUrl', function($http, $stateParams, ApiUrl) {
            return $http.get(ApiUrl(`/users`))
              .then((resp) => {
                return resp.data.users;
              })
              .catch(() => {
                return [];
              });
          }]
        }
      })
      .state('main.admin.smtp_setup', {
        url: 'smtp-setup',
        resolve: {
          currentUser: ['$http', 'CurrentUser', ($http, CurrentUser) => {
            return CurrentUser.user();
          }],
          smtp: ['currentUser', '$http', 'ApiUrl', 'Session', (currentUser, $http, ApiUrl) => {
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
        }
      })
      .state('main.admin.saml_setup', {
        url: 'saml-setup',
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
        }
      });
  })
  .name;
