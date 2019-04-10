import ng from 'angular';
import { capitalize } from 'lodash';

function capitalizeFilter() {
  return capitalize;
}

export default ng
  .module('cd.common.filters.captialize', [])
  .filter('capitalize', capitalizeFilter)
  .name;
