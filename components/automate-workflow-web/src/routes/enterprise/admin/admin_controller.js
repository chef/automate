import ng from 'angular';
import Breadcrumb from '../../../common/ui/breadcrumb/breadcrumb';

adminController.$inject = ['$scope', 'Breadcrumb', 'users'];

function adminController($scope, Breadcrumb, users) {
  Breadcrumb.setCrumb([]);

  $scope.users = users;
}

export default ng
  .module('cd.routes.enterprise.admin.controller', [
    Breadcrumb
  ])
  .controller('adminController', adminController)
  .name;
