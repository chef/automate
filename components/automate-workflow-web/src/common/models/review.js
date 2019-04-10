import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';

Review.$inject = ['restmod'];

function Review(restmod) {
  var def = {};

  return restmod.model(null, def);
}

export default ng
  .module('cd.common.models.review', [
    'restmod'
  ])
  .factory('Review', Review)
  .name;
