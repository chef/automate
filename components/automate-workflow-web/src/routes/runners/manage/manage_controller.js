import ng from 'angular';
import Session from '../../../common/auth/session';

manageController.$inject = [
  '$scope',
  'runners',
  'Session',
  '$http',
  '$httpParamSerializer',
  '$window'
];

function manageController($scope, runners, Session, $http, $httpParamSerializer, $window) {
  $scope.runners = runners;

  $scope.stream = undefined;

  let { enterprise, username, token } = Session.get();
  let streamPath = `/workflow/api/v0/e/${enterprise}/runners/streaming`;
  let streamParams = $httpParamSerializer({
      'chef-delivery-user': username,
      'chef-delivery-token': token
  });
  let streamUrl = `${streamPath}?${streamParams}`;

  $scope.handleRunnersUpdateEvent = (e) => {
    $scope.runners = JSON.parse(e.data);
  };

  $scope.initStream = function () {
    $scope.stream = new $window.EventSource(streamUrl);
    $scope.stream.addEventListener('runners_state_updated', $scope.handleRunnersUpdateEvent);
  };

  $scope.closeStream = function () {
    $scope.stream.removeEventListener('runners_state_updated', $scope.handleRunnersUpdateEvent);
    $scope.stream.close();
  };

  $scope.initStream();

  $scope.$on('$destroy', () => {
    $scope.closeStream();
  });
}

export default ng
  .module('cd.routes.runners.manage.controller', [
    Session
  ])
  .controller('manageController', manageController)
  .name;
