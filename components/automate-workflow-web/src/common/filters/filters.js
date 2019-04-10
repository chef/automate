import ng from 'angular';
import capitalize from './capitalize';
import filterBy from './filter_by';
import moment from './moment';

export default ng
  .module('cd.common.filters', [
    capitalize,
    filterBy,
    moment
  ])
  .name;
