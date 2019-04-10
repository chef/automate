import ng from 'angular';
import rolesFieldTemplate from './roles_field.html';

var roles = ['admin', 'committer', 'reviewer', 'shipper', 'observer'];

function rolesFieldComponent() {

  function link(scope, element, attrs) {

    scope.roles = roles;

    scope.toggleSelectedRole = function (role) {
      var index = scope.user.roles.indexOf(role);
      if (index > -1) {
        scope.user.roles.splice(index, 1);
      } else {
        scope.user.roles.push(role);
      }
    };

    scope.hasRole = function (role) {
      return !!scope.user.roles && scope.user.roles.indexOf(role) > -1;
    };
  }

  return {
    link: link,
    template: rolesFieldTemplate
  };
}

export default ng
  .module('cd.components.rolesField', [])
  .directive('cdRolesField', rolesFieldComponent)
  .name;
