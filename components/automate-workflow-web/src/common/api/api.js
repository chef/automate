import ng from 'angular';
import ApiInterceptor from './api_interceptor';
import ApiUrl from './api_url';

export default ng
  .module('cd.common.api', [
    ApiInterceptor,
    ApiUrl
  ])
  .name;
