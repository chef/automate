import ng from 'angular';
import { filter } from 'lodash';

function filterByFilter() {
  return function (input, filterString) {
    if (filterString === 'total') {
      return input;
    } else {
      return filter(input, { state: filterString });
    }
  };
}

export default ng
  .module('cd.common.filters.filterBy', [])
  .filter('filterBy', filterByFilter)
  .name;
