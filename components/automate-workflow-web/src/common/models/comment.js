import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import { find, filter } from 'lodash';

Comment.$inject = ['restmod'];

function Comment(restmod) {

  var def = {
    '~after-feed': function () {
      Object.defineProperty(this, 'children', {
        get: function () {
          return filter(this.$scope, { parentId: this.id });
        }
      });

      Object.defineProperty(this, 'root', {
        get: function () {
          var parent = find(this.$scope, { id: this.parentId });
          if (parent) {
            return parent.root;
          } else {
            return this;
          }
        }
      });
    }
  };

  return restmod.model(null, def);
}

export default ng
  .module('cd.common.models.comment', [
    'restmod'
  ])
  .factory('Comment', Comment)
  .name;
