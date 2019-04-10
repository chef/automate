import ng from 'angular';
import api from './api/api';
import auth from './auth/auth';
import featureFlags from './feature_flags/feature_flags';
import filters from './filters/filters';
import models from './models/models';
import projects from './projects/projects';
import ui from './ui/ui';
import users from './users/users';
import ConfigService from './config/config_service';

export default ng
  .module('cd.common', [
    api,
    auth,
    featureFlags,
    filters,
    models,
    projects,
    ui,
    users,
    ConfigService
  ])
  .name;
