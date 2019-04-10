import ng from 'angular';
import uiRouter from 'angular-ui-router';

Breadcrumb.$inject = ['$rootScope', '$state', '$stateParams'];

function Breadcrumb($rootScope, $state, $stateParams) {
  let breadcrumb = [];

  return {
    setCrumb: function (crumb) {
      breadcrumb = crumb;
      $rootScope.$emit('updateBreadcrumb');
    },

    getCrumb: function () {
      return breadcrumb;
    }
  };
}

export default ng
  .module('cd.common.ui.breadcrumb.service', [
    uiRouter
  ])
  .service('Breadcrumb', Breadcrumb)
  .name;
