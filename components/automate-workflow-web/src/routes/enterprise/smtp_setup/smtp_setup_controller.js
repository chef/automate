import ng from 'angular';

smtpSetupController.$inject = ['$scope', 'smtp'];

function smtpSetupController($scope, smtp) {
  $scope.smtp = smtp;
}

export default ng
  .module('cd.routes.enterprise.smtp_setup.controller', [])
  .controller('smtpSetupController', smtpSetupController)
  .name;
