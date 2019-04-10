import ng from 'angular';

auditController.$inject = ['$scope', 'auditLog'];

function auditController($scope, auditLog) {
  $scope.auditLog = auditLog;
}

export default ng
  .module('cd.routes.audit.controller', [])
  .controller('auditController', auditController)
  .name;
