import ng from 'angular';
import uiRouter from 'angular-ui-router';
import audit from './audit/audit';
import authenticate from './authenticate/authenticate';
import change from './change/change';
import enterprise from './enterprise/enterprise';
import main from './main/main';
import organization from './organization/organization';
import project from './project/project';
import redirects from './redirects';
import runners from './runners/runners';

export default ng
  .module('cd.routes', [
    uiRouter,
    audit,
    authenticate,
    change,
    enterprise,
    main,
    organization,
    project,
    redirects,
    runners
  ])
  .config(($urlRouterProvider) => {
    $urlRouterProvider.when('/viz/{hash:.*}', function($match, $stateParams) {
      window.location.href = "/workflow";
    });
    $urlRouterProvider.otherwise(function($injector, $location) {
      $location.url('/dashboard');
    });
  })
  .run(($log, $rootScope) => {
    $rootScope.$on('$stateChangeError',
      (event, toState, toParams, fromState, fromParams, error) => {
        $log.error(event, error);
      });
    $rootScope.$on('$stateChangeSuccess', function() {
      document.body.scrollTop = document.documentElement.scrollTop = 0;
      });
  })
  .name;
