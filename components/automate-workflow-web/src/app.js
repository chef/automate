import 'reflect-metadata';
require('zone.js/dist/zone');
require('zone.js/dist/long-stack-trace-zone');

import ng from 'angular';
import config from './config';
import version from './version';
import common from './common/common';
import components from './components/components';
import routes from './routes/routes';
import ConfigService from './common/config/config_service';

import { upgradeAdapter } from './upgrade-adapter';
import { ResetPasswordComponent } from './components/reset_password/reset_password.component';

import './stylesheets/screen.scss';

export default ng
  .module('cd', [
    config,
    common,
    components,
    routes
  ])
  .run(function(ConfigService) { ConfigService.setConfig(); })
  .constant('appVersion', version)
  .directive('resetPassword', upgradeAdapter.downgradeNg2Component(ResetPasswordComponent))
  .name;

upgradeAdapter.bootstrap(document.querySelector('html'), ['cd']);
