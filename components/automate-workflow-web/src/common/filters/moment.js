import ng from 'angular';
import moment from 'moment';

function momentFilter() {
  return function (input, formatString) {
    return moment(input).format(formatString);
  };
}

export default ng
  .module('cd.common.filters.moment', [])
  .filter('moment', momentFilter)
  .name;
