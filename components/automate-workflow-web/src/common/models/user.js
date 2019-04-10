import ng from 'angular';
import { contains, some } from 'lodash';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import NamedModelPacker from './named_model_packer';
import ApiUrl from '../api/api_url';

User.$inject = ['restmod', 'ApiUrl'];

function User(restmod, ApiUrl) {
  
  var def = {
    PACKER: 'namedModel',

    canShip: function () {
      return some(this.roles, function (role) {
        return contains(['admin', 'shipper'], role);
      });
    }
  };

  return restmod.model('/users', def, function () {
    this.setPrimaryKey('name');
    this.setUrlPrefix(ApiUrl('/'));
  });
}

export default ng
  .module('cd.common.models.user', [
    'restmod',
    ApiUrl,
    NamedModelPacker
  ])
  .factory('User', User)
  .name;
