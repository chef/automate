import ng from 'angular';
import Session from '../../../common/auth/session';

queueController.$inject = [
  '$scope',
  'jobs',
  'Session',
  '$http',
  '$httpParamSerializer',
  '$window'
];

function queueController($scope, jobs, Session, $http, $httpParamSerializer, $window) {
  $scope.jobs = jobs;

  // Create the stream
  $scope.stream = undefined;

  // Construct stream URL
  let { enterprise, username, token } = Session.get();
  let streamPath = `/workflow/api/v0/e/${enterprise}/jobs/streaming`;
  let streamParams = $httpParamSerializer({
    'chef-delivery-user': username,
    'chef-delivery-token': token
  });
  let streamUrl = `${streamPath}?${streamParams}`;

  $scope.handleUpdateJobQueueEvent = (e) => {
    $scope.jobs = JSON.parse(e.data);
  };

  $scope.initStream = function () {
    $scope.stream = new $window.EventSource(streamUrl);
    $scope.stream.addEventListener('update_job_queue', $scope.handleUpdateJobQueueEvent);
  };

  $scope.closeStream = function () {
    $scope.stream.removeEventListener('update_job_queue', $scope.handleUpdateJobQueueEvent);
    $scope.stream.close();
  };

  $scope.initStream();

  $scope.$on('$destroy', () => {
    $scope.closeStream();
  });
}

export default ng
  .module('cd.routes.runners.queue.controller', [
    Session
  ])
  .controller('queueController', queueController)
  .name;
