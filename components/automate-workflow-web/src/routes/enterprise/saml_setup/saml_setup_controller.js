import ng from 'angular';

samlSetupController.$inject = ['$scope', 'saml'];

function samlSetupController($scope, saml) {
  $scope.saml = saml;
}

export default ng
  .module('cd.routes.enterprise.saml_setup.controller', [])
  .controller('samlSetupController', samlSetupController)
  .name;
