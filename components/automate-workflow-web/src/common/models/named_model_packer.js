import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import { isObject } from 'lodash';

NamedModelPacker.$inject = ['restmod'];

function NamedModelPacker() {

  function Packer(model) {
    this.plural = model.$getProperty('plural');
  }

  Packer.prototype = {

    unpack: function (unpackedRaw, model) {
      return unpackedRaw;
    },

    unpackMany: function (unpackedRaw, collection) {
      collection.names = unpackedRaw[this.plural];

      if (isObject(collection.names[0])) return collection.names;

      return collection.names.map(function (name) {
        return { name: name };
      });
    },

    pack: function (raw) {
      return raw;
    }
  };

  return function (model) {
    return new Packer(model);
  };
}

export default ng
  .module('cd.common.models.namedModelPacker', [
    'restmod'
  ])
  .factory('NamedModelPacker', NamedModelPacker)
  .name;
