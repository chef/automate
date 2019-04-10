import ng from 'angular';
import uiRouter from 'angular-ui-router';
import AuthInitializer from './auth_initializer';
import AuthInterceptor from './auth_interceptor';
import AuthService from './auth_service';
import Session from './session';

export default ng
  .module('cd.common.auth', [
    uiRouter,
    AuthInitializer,
    AuthInterceptor,
    AuthService,
    Session
  ])
  .name;
