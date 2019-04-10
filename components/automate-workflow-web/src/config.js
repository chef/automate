import ng from 'angular';
import appConfig from 'app_config';

export default ng
  .module('cd.appConfig', [])
  .constant('appConfig', appConfig)
  .name;
