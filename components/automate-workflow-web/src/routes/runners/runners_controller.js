import ng from 'angular';

runnersController.$inject = ['$scope'];

function runnersController($scope) {}

export default ng
  .module('cd.routes.runners.controller', [])
  .controller('runnersController', runnersController)
  .name;
