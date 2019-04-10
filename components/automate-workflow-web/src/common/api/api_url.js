import ng from 'angular';
import { has } from 'lodash';
import appConfig from '../../config';
import Session from '../auth/session';

ApiUrl.$inject = ['appConfig', 'Session'];

function ApiUrl(appConfig, Session) {
  let apiObj = appConfig.api;
  let base = () => apiObj.base_url + apiObj.version + '/e/' + Session.get('enterprise');

  function getUrl(endpoint) {
    if (has(apiObj.endpoints, endpoint)) {
      return base() + apiObj.endpoints[endpoint];
    } else {
      return base() + endpoint;
    }
  }

  return getUrl;
}

export default ng
  .module('cd.common.api.api_url', [
    appConfig,
    Session,
  ])
  .service('ApiUrl', ApiUrl)
  .name;
