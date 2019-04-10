import ng from 'angular';

welcomeController.$inject = ['$scope', '$window'];

function welcomeController($scope, $window) {

  $scope.reload = () => {
    $window.location.href = '/workflow';
  };
}

export default ng
  .module('cd.routes.authenticate.welcome.controller', [])
  .controller('welcomeController', welcomeController)
  .name;
