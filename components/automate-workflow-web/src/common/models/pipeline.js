import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import NamedModelPacker from './named_model_packer';
import Change from './change';

Pipeline.$inject = ['restmod'];

function Pipeline(restmod) {
  var def = {
    PACKER: 'namedModel',
    PLURAL: 'pipelines',
    changes: { hasMany: 'Change' }
  };

  return restmod.model(null, def, function () {
    this.setPrimaryKey('name');
  });
}

export default ng
  .module('cd.common.models.pipeline', [
    'restmod',
    NamedModelPacker,
    Change
  ])
  .factory('Pipeline', Pipeline)
  .name;
