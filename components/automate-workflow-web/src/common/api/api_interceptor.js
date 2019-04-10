import ng from 'angular';
import Flash from '../ui/flash/flash';

apiInterceptor.$inject = ['$q', 'Flash'];

// The ignoreApiInterceptor config option below is useful when we want to display
// custom error messages from 500 errors returned by the server. Without that,
// double error messages show up. See scm_setup_controller.js for an example of
// how to use it.
function apiInterceptor($q, Flash) {
  return {
    responseError: function (resp) {
      if (resp.status === 500 && !resp.config.ignoreApiInterceptor) {
        Flash.error('Error', 'Bad server response');
      }

      return $q.reject(resp);
    }
  };
}

apiConfig.$inject = ['$httpProvider'];

function apiConfig($httpProvider) {
  $httpProvider.interceptors.push('apiInterceptor');
}

export default ng
  .module('cd.common.api.interceptor', [
    Flash
  ])
  .factory('apiInterceptor', apiInterceptor)
  .config(apiConfig)
  .name;
