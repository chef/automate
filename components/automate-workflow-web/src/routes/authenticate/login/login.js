import ng from 'angular';
import ngCookies from 'angular-cookies';
import uiRouter from 'angular-ui-router';
import loginController from './login_controller';
import loginComponent from './login_component';
import featureFlags from '../../../common/feature_flags/feature_flags';

export default ng
  .module('cd.routes.authenticate.login', [
    featureFlags,
    uiRouter,
    loginController,
    loginComponent,
    ngCookies
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('authenticate.login', {
        url: '/login?returnUrl',
        template: '<cd-login>',
        controller: 'loginController',
        data: {
          title: 'Login'
        }
      });
  })
  .name;
