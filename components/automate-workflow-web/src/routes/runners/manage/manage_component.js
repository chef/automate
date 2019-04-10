import ng from 'angular';
import manageTemplate from './manage.html';
import ApiUrl from '../../../common/api/api_url';

manageComponent.$inject = ['$http', 'ApiUrl', 'Flash'];

function manageComponent($http, ApiUrl, Flash) {
  function link(scope) {

    let errorIcon = ['fa-exclamation-triangle', 'runner-status-error'];
    let successIcon = ['fa-check-circle', 'runner-status-ok'];
    let inProgressIcon = ['fa-spinner', 'fa-pulse', 'runner-status-in-progress'];
    let unknownIcon = ['fa-minus', 'runner-status-unknown'];

    scope.canTest = (runner) =>
      runner.health.status === 'pending' || !!runner.job.id;

    scope.healthIcon = (health) => {
      if (!health.status) {
        return unknownIcon;
      }
      if (health.status === 'ok') {
        return successIcon;
      }
      if (health.status === 'error') {
        return errorIcon;
      }
      if (health.status === 'pending') {
        return inProgressIcon;
      }
    };

    scope.healthOutput = (health) => {
      if (!health) {
        return "";
      }
      if (health.status === 'ok') {
        return "Output";
      }
      if (health.status === 'error') {
        return "Error";
      }
      return "";
    };

    scope.checkStatus = (runner) => {
      let statusUrl = ApiUrl(`/runners/${runner.hostname}/health`);
      $http.post(statusUrl, {})
        .then(
            (resp) => {
              runner.health = resp.data.health;
            },
            (error) => {
              Flash.error('Error', 'Could not initiate health check');
            });
    };
  }
  return {
    link: link,
    template: manageTemplate,
    scope: {
      runners: '='
    }
  };
}

export default ng
  .module('cd.routes.runners.manage.component', [
    ApiUrl
  ])
  .directive('cdRunnersManage', manageComponent)
  .name;
